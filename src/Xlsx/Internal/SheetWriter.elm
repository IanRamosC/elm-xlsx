module Xlsx.Internal.SheetWriter exposing (State, initialState, write)

{-| Write one worksheet part.

The elements of `CT_Worksheet` must appear in schema order, or Excel offers to
"repair" the file. This module writes them in that order and leaves the
optional slots for later versions empty:

    sheetPr → dimension → sheetViews → sheetFormatPr → cols → sheetData →
    sheetProtection → autoFilter → mergeCells → conditionalFormatting →
    dataValidations → hyperlinks → printOptions → pageMargins → pageSetup →
    headerFooter → drawing → tableParts → extLst

Shared strings and cell formats are collected across sheets, so they are
threaded through as `State`.

@docs State, initialState, write

-}

import Dict exposing (Dict)
import Xlsx.Date as XDate
import Xlsx.Internal.FutureFunctions as FutureFunctions
import Xlsx.Internal.Limits as Limits
import Xlsx.Internal.Model as Model exposing (Cell(..), ColumnData, Problem, Row(..), SheetData, StyleData, Value(..))
import Xlsx.Internal.NumFmt as NumFmt
import Xlsx.Internal.Opc as Opc
import Xlsx.Internal.SharedStrings as SharedStrings
import Xlsx.Internal.Styles as Styles
import Xlsx.Internal.Xml.Escape as Escape
import Xlsx.Ref as Ref


{-| Workbook-wide data collected while writing sheets. `problems` is in
reverse order.
-}
type alias State =
    { strings : SharedStrings.Table
    , styles : Styles.Registry
    , problems : List Problem
    , needsRecalc : Bool
    }


{-| -}
initialState : State
initialState =
    { strings = SharedStrings.empty
    , styles = Styles.empty
    , problems = []
    , needsRecalc = False
    }


{-| What every cell of a sheet needs to know.
-}
type alias Context =
    { sheetName : String
    , columnStyles : Dict Int StyleData
    , merges : List Ref.Range
    }


type alias Acc =
    { rowNumber : Int
    , out : List String
    , block : List PendingRow
    , extent : Maybe Ref.Range
    , rowLimitReported : Bool
    , state : State
    }


{-| The worksheet XML. `selected` marks the active sheet's tab.
-}
write : { selected : Bool } -> SheetData -> State -> ( String, State )
write { selected } sheet initial =
    let
        problem message =
            { sheet = Just sheet.name, cell = Nothing, message = message }

        ( merges, mergeProblems ) =
            parseMerges sheet.merges

        ( autoFilter, autoFilterProblems ) =
            case sheet.autoFilter of
                Nothing ->
                    ( Nothing, [] )

                Just text ->
                    case Ref.rangeFromString text of
                        Just r ->
                            ( Just r, [] )

                        Nothing ->
                            ( Nothing, [ "The autofilter range \"" ++ text ++ "\" is not a valid range like A1:D20." ] )

        sheetProblems =
            List.map problem (columnProblems sheet.columns ++ freezeProblems sheet.freeze ++ mergeProblems ++ autoFilterProblems)

        ( colsXml, withColumns ) =
            columnsXml sheet.columns { initial | problems = List.reverse sheetProblems ++ initial.problems }

        context =
            { sheetName = sheet.name
            , columnStyles = Dict.foldl (\k col styles -> Maybe.map (\st -> Dict.insert k st styles) col.style |> Maybe.withDefault styles) Dict.empty sheet.columns
            , merges = merges
            }

        acc =
            List.foldl (writeRow context)
                { rowNumber = 1, out = [], block = [], extent = Nothing, rowLimitReported = False, state = withColumns }
                sheet.rows
                |> flushBlock

        dimension =
            acc.extent
                |> Maybe.map Ref.rangeToString
                |> Maybe.withDefault "A1"

        sheetData =
            if List.isEmpty acc.out then
                "<sheetData/>"

            else
                "<sheetData>" ++ String.concat (List.reverse acc.out) ++ "</sheetData>"
    in
    ( String.concat
        [ Opc.xmlDeclaration
        , "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\""
        , " xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">"
        , "<dimension ref=\"" ++ dimension ++ "\"/>"
        , sheetViewsXml selected sheet.freeze
        , "<sheetFormatPr defaultRowHeight=\"15\"/>"
        , colsXml
        , sheetData
        , case autoFilter of
            Just r ->
                "<autoFilter ref=\"" ++ Ref.rangeToString r ++ "\"/>"

            Nothing ->
                ""
        , mergeCellsXml merges
        , "<pageMargins left=\"0.7\" right=\"0.7\" top=\"0.75\" bottom=\"0.75\" header=\"0.3\" footer=\"0.3\"/>"
        , "</worksheet>"
        ]
    , acc.state
    )



-- SHEET VIEWS AND PANES


sheetViewsXml : Bool -> Maybe { rows : Int, columns : Int } -> String
sheetViewsXml selected freeze =
    let
        open =
            "<sheetViews><sheetView "
                ++ (if selected then
                        "tabSelected=\"1\" "

                    else
                        ""
                   )
                ++ "workbookViewId=\"0\""
    in
    case freeze of
        Just { rows, columns } ->
            if rows > 0 || columns > 0 then
                open ++ ">" ++ paneXml rows columns ++ "</sheetView></sheetViews>"

            else
                open ++ "/></sheetViews>"

        Nothing ->
            open ++ "/></sheetViews>"


{-| A frozen pane plus the selections Excel writes for it.
-}
paneXml : Int -> Int -> String
paneXml rows columns =
    let
        topLeft =
            Ref.toString { column = columns + 1, row = rows + 1 }

        split =
            (if columns > 0 then
                " xSplit=\"" ++ String.fromInt columns ++ "\""

             else
                ""
            )
                ++ (if rows > 0 then
                        " ySplit=\"" ++ String.fromInt rows ++ "\""

                    else
                        ""
                   )

        pane active =
            "<pane" ++ split ++ " topLeftCell=\"" ++ topLeft ++ "\" activePane=\"" ++ active ++ "\" state=\"frozen\"/>"

        selection paneName ref =
            "<selection pane=\"" ++ paneName ++ "\" activeCell=\"" ++ ref ++ "\" sqref=\"" ++ ref ++ "\"/>"
    in
    if rows > 0 && columns > 0 then
        pane "bottomRight"
            ++ selection "topRight" (Ref.toString { column = columns + 1, row = 1 })
            ++ selection "bottomLeft" (Ref.toString { column = 1, row = rows + 1 })
            ++ "<selection pane=\"bottomRight\"/>"

    else if rows > 0 then
        pane "bottomLeft" ++ "<selection pane=\"bottomLeft\"/>"

    else
        pane "topRight" ++ "<selection pane=\"topRight\"/>"


freezeProblems : Maybe { rows : Int, columns : Int } -> List String
freezeProblems freeze =
    case freeze of
        Just { rows, columns } ->
            if rows < 0 || columns < 0 then
                [ "Frozen rows and columns cannot be negative." ]

            else if rows >= Limits.maxRows || columns >= Limits.maxColumns then
                [ "Too many frozen rows or columns." ]

            else
                []

        Nothing ->
            []



-- COLUMNS


{-| Excel's column width unit is the width of a digit in the default font
(Calibri 11: 7 pixels), plus 5 pixels of padding. Convert the width a user
sees in Excel to the stored value, as XlsxWriter does.
-}
storedWidth : Float -> Float
storedWidth width =
    let
        maxDigitWidth =
            7

        padding =
            5

        pixels =
            if width < 1 then
                toFloat (round (width * (maxDigitWidth + padding)))

            else
                toFloat (round (width * maxDigitWidth) + padding)
    in
    toFloat (truncate (pixels / maxDigitWidth * 256)) / 256


{-| Excel's default column width.
-}
defaultWidth : Float
defaultWidth =
    8.43


columnProblems : Dict Int ColumnData -> List String
columnProblems columns =
    Dict.foldr
        (\index col acc ->
            (if index < 1 || index > Limits.maxColumns then
                [ "Column " ++ String.fromInt index ++ " is outside Excel's columns (1 to " ++ String.fromInt Limits.maxColumns ++ ")." ]

             else
                []
            )
                ++ (case col.width of
                        Just w ->
                            if isNaN w || w < 0 || w > 255 then
                                [ "Column " ++ Ref.columnName index ++ " has width " ++ String.fromFloat w ++ "; widths go from 0 to 255." ]

                            else
                                []

                        Nothing ->
                            []
                   )
                ++ acc
        )
        []
        columns


type alias ColumnRun =
    { min : Int
    , max : Int
    , key : ( Maybe Float, Bool, Int )
    }


{-| `<cols>`, with neighboring columns that have identical settings merged into
one `<col min max>`.
-}
columnsXml : Dict Int ColumnData -> State -> ( String, State )
columnsXml columns state =
    let
        ( runs, finalState ) =
            Dict.foldl
                (\index col ( acc, st ) ->
                    if index < 1 || index > Limits.maxColumns then
                        ( acc, st )

                    else
                        let
                            ( xf, styles ) =
                                case col.style of
                                    Just style ->
                                        Styles.register style st.styles

                                    Nothing ->
                                        ( 0, st.styles )

                            key =
                                ( col.width, col.hidden, xf )
                        in
                        ( case acc of
                            run :: rest ->
                                if run.max == index - 1 && run.key == key then
                                    { run | max = index } :: rest

                                else
                                    { min = index, max = index, key = key } :: acc

                            [] ->
                                [ { min = index, max = index, key = key } ]
                        , { st | styles = styles }
                        )
                )
                ( [], state )
                columns
    in
    if List.isEmpty runs then
        ( "", finalState )

    else
        ( "<cols>" ++ String.concat (List.map colXml (List.reverse runs)) ++ "</cols>", finalState )


colXml : ColumnRun -> String
colXml run =
    let
        ( width, hidden, xf ) =
            run.key
    in
    String.concat
        [ "<col min=\"" ++ String.fromInt run.min ++ "\" max=\"" ++ String.fromInt run.max ++ "\""
        , " width=\"" ++ String.fromFloat (storedWidth (Maybe.withDefault defaultWidth width)) ++ "\""
        , if xf /= 0 then
            " style=\"" ++ String.fromInt xf ++ "\""

          else
            ""
        , if hidden then
            " hidden=\"1\""

          else
            ""
        , if width /= Nothing then
            " customWidth=\"1\""

          else
            ""
        , "/>"
        ]



-- MERGED CELLS


parseMerges : List String -> ( List Ref.Range, List String )
parseMerges texts =
    let
        ( ranges, problems ) =
            List.foldl
                (\text ( okAcc, errAcc ) ->
                    case Ref.rangeFromString text of
                        Nothing ->
                            ( okAcc, ("The merge range \"" ++ text ++ "\" is not a valid range like A1:C1.") :: errAcc )

                        Just r ->
                            if r.start == r.end then
                                ( okAcc, ("The merge range " ++ text ++ " covers a single cell; merges need at least two.") :: errAcc )

                            else
                                case List.filter (overlaps r) okAcc of
                                    other :: _ ->
                                        ( okAcc, ("The merge range " ++ Ref.rangeToString r ++ " overlaps " ++ Ref.rangeToString other ++ ".") :: errAcc )

                                    [] ->
                                        ( r :: okAcc, errAcc )
                )
                ( [], [] )
                texts
    in
    ( List.reverse ranges, List.reverse problems )


overlaps : Ref.Range -> Ref.Range -> Bool
overlaps a b =
    a.start.column <= b.end.column && b.start.column <= a.end.column && a.start.row <= b.end.row && b.start.row <= a.end.row


mergeCellsXml : List Ref.Range -> String
mergeCellsXml merges =
    if List.isEmpty merges then
        ""

    else
        "<mergeCells count=\""
            ++ String.fromInt (List.length merges)
            ++ "\">"
            ++ String.concat (List.map (\r -> "<mergeCell ref=\"" ++ Ref.rangeToString r ++ "\"/>") merges)
            ++ "</mergeCells>"


{-| Only the top-left cell of a merged range shows its value. The others keep
their style (so borders still draw) but lose their value.
-}
hiddenByMerge : List Ref.Range -> Ref.Ref -> Bool
hiddenByMerge merges ref =
    List.any (\r -> Ref.rangeContains ref r && r.start /= ref) merges



-- ROWS


writeRow : Context -> Row -> Acc -> Acc
writeRow context row acc =
    case row of
        EmptyRows n Nothing ->
            { acc | rowNumber = acc.rowNumber + max 0 n }

        EmptyRows n (Just height) ->
            List.foldl
                (\_ a -> writeRow context (Row { startColumn = 1, cells = [], height = Just height }) a)
                acc
                (List.range 1 n)

        Row data ->
            if acc.rowNumber > Limits.maxRows then
                if acc.rowLimitReported then
                    acc

                else
                    { acc
                        | rowLimitReported = True
                        , state = addProblem (sheetProblem context ("The sheet has more than " ++ String.fromInt Limits.maxRows ++ " rows, Excel's limit.")) acc.state
                    }

            else if data.startColumn < 1 then
                { acc
                    | rowNumber = acc.rowNumber + 1
                    , state = addProblem (sheetProblem context ("Row " ++ String.fromInt acc.rowNumber ++ " starts at column " ++ String.fromInt data.startColumn ++ "; columns start at 1.")) acc.state
                }

            else
                let
                    heightProblems =
                        case data.height of
                            Just h ->
                                if isNaN h || h < 0 || h > 409 then
                                    [ sheetProblem context ("Row " ++ String.fromInt acc.rowNumber ++ " has height " ++ String.fromFloat h ++ "; heights go from 0 to 409 points.") ]

                                else
                                    []

                            Nothing ->
                                []

                    rowAcc =
                        List.foldl (writeCell context acc.rowNumber)
                            { column = data.startColumn, cells = [], first = Nothing, last = Nothing, columnLimitReported = False, state = List.foldl addProblem acc.state heightProblems }
                            data.cells

                    heightAttrs =
                        case data.height of
                            Just h ->
                                " ht=\"" ++ String.fromFloat h ++ "\" customHeight=\"1\""

                            Nothing ->
                                ""

                    rowNumberText =
                        String.fromInt acc.rowNumber
                in
                case ( rowAcc.first, rowAcc.last ) of
                    ( Just first, Just last ) ->
                        { acc
                            | rowNumber = acc.rowNumber + 1
                            , extent = Just (grow acc.extent { column = first, row = acc.rowNumber } { column = last, row = acc.rowNumber })
                            , state = rowAcc.state
                        }
                            |> pushRow { number = acc.rowNumber, attrs = heightAttrs, cells = rowAcc.cells, span = Just ( first, last ) }

                    _ ->
                        if data.height /= Nothing then
                            -- Excel counts rows that only have a height in the dimension.
                            { acc
                                | rowNumber = acc.rowNumber + 1
                                , extent = Just (grow acc.extent { column = 1, row = acc.rowNumber } { column = 1, row = acc.rowNumber })
                                , state = rowAcc.state
                            }
                                |> pushRow { number = acc.rowNumber, attrs = heightAttrs, cells = [], span = Nothing }

                        else
                            { acc | rowNumber = acc.rowNumber + 1, state = rowAcc.state }


{-| A written row waiting for its block of 16 rows to be complete. `cells` is
in reverse order.
-}
type alias PendingRow =
    { number : Int
    , attrs : String
    , cells : List String
    , span : Maybe ( Int, Int )
    }


{-| Excel's optional `spans` hint covers blocks of 16 rows: every row in a
block gets the first and last used column of the whole block. So rows wait
until their block is complete.
-}
pushRow : PendingRow -> Acc -> Acc
pushRow pending acc =
    case acc.block of
        previous :: _ ->
            if (previous.number - 1) // 16 == (pending.number - 1) // 16 then
                { acc | block = pending :: acc.block }

            else
                { acc | block = [ pending ], out = blockXml acc.block acc.out }

        [] ->
            { acc | block = [ pending ] }


flushBlock : Acc -> Acc
flushBlock acc =
    { acc | block = [], out = blockXml acc.block acc.out }


{-| Prepend a block's rows (given newest first) to the reversed output.
-}
blockXml : List PendingRow -> List String -> List String
blockXml block out =
    let
        spans =
            List.filterMap .span block
                |> List.foldl
                    (\( first, last ) acc ->
                        case acc of
                            Just ( f, l ) ->
                                Just ( min f first, max l last )

                            Nothing ->
                                Just ( first, last )
                    )
                    Nothing
                |> Maybe.map (\( f, l ) -> " spans=\"" ++ String.fromInt f ++ ":" ++ String.fromInt l ++ "\"")
                |> Maybe.withDefault ""
    in
    List.foldr
        (\row acc ->
            if List.isEmpty row.cells then
                ("<row r=\"" ++ String.fromInt row.number ++ "\"" ++ row.attrs ++ "/>") :: acc

            else
                "</row>" :: row.cells ++ (("<row r=\"" ++ String.fromInt row.number ++ "\"" ++ spans ++ row.attrs ++ ">") :: acc)
        )
        out
        block


sheetProblem : Context -> String -> Problem
sheetProblem context message =
    { sheet = Just context.sheetName, cell = Nothing, message = message }


grow : Maybe Ref.Range -> Ref.Ref -> Ref.Ref -> Ref.Range
grow extent a b =
    case extent of
        Nothing ->
            Ref.range a b

        Just r ->
            Ref.range
                { column = min r.start.column a.column, row = min r.start.row a.row }
                { column = max r.end.column b.column, row = max r.end.row b.row }


type alias RowAcc =
    { column : Int
    , cells : List String
    , first : Maybe Int
    , last : Maybe Int
    , columnLimitReported : Bool
    , state : State
    }


writeCell : Context -> Int -> Cell -> RowAcc -> RowAcc
writeCell context rowNumber (Cell cell) acc =
    if acc.column > Limits.maxColumns then
        if acc.columnLimitReported then
            acc

        else
            { acc
                | columnLimitReported = True
                , state = addProblem (sheetProblem context ("Row " ++ String.fromInt rowNumber ++ " has cells past column XFD, Excel's last column.")) acc.state
            }

    else
        let
            position =
                { column = acc.column, row = rowNumber }

            ref =
                Ref.toString position

            value =
                if not (List.isEmpty context.merges) && hiddenByMerge context.merges position then
                    Blank

                else
                    cell.value

            style =
                case Dict.get acc.column context.columnStyles of
                    Just columnStyle ->
                        Model.mergeStyles columnStyle cell.style

                    Nothing ->
                        cell.style

            problems =
                Limits.checkCell context.sheetName ref value

            state =
                List.foldl addProblem acc.state problems
        in
        case cellXml ref value (effectiveStyle value style) state of
            ( Just xml, newState ) ->
                { acc
                    | column = acc.column + 1
                    , cells = xml :: acc.cells
                    , first =
                        if acc.first == Nothing then
                            Just acc.column

                        else
                            acc.first
                    , last = Just acc.column
                    , state = newState
                }

            ( Nothing, newState ) ->
                { acc | column = acc.column + 1, state = newState }


addProblem : Problem -> State -> State
addProblem problem state =
    { state | problems = problem :: state.problems }


{-| Date cells without a number format get a default one, so they do not show
up as bare serial numbers.
-}
effectiveStyle : Value -> StyleData -> StyleData
effectiveStyle value style =
    case ( style.numberFormat, value ) of
        ( Nothing, DateValue _ ) ->
            { style | numberFormat = Just NumFmt.defaultDate }

        ( Nothing, DateTimeValue _ ) ->
            { style | numberFormat = Just NumFmt.defaultDateTime }

        ( Nothing, Formula _ (Just cached) ) ->
            effectiveStyle cached style

        _ ->
            style


cellXml : String -> Value -> StyleData -> State -> ( Maybe String, State )
cellXml ref value style state =
    let
        ( xf, withStyle ) =
            Styles.register style state.styles

        styledState =
            { state | styles = withStyle }

        open typeAttr =
            "<c r=\""
                ++ ref
                ++ "\""
                ++ (if xf == 0 then
                        ""

                    else
                        " s=\"" ++ String.fromInt xf ++ "\""
                   )
                ++ typeAttr
                ++ ">"

        simple typeAttr v =
            open typeAttr ++ "<v>" ++ v ++ "</v></c>"
    in
    case value of
        Text s ->
            let
                ( index, strings ) =
                    SharedStrings.insert s styledState.strings
            in
            ( Just (simple " t=\"s\"" (String.fromInt index)), { styledState | strings = strings } )

        Number n ->
            ( Just (simple "" (number n)), styledState )

        Boolean b ->
            ( Just (simple " t=\"b\"" (boolValue b)), styledState )

        DateValue d ->
            ( Just (simple "" (String.fromInt (XDate.dateToSerial XDate.Excel1900 d))), styledState )

        DateTimeValue dt ->
            ( Just (simple "" (number (XDate.dateTimeToSerial XDate.Excel1900 dt))), styledState )

        ErrorValue code ->
            ( Just (simple " t=\"e\"" (Escape.text code)), styledState )

        Formula f cached ->
            let
                formulaXml =
                    "<f>" ++ Escape.text (formulaText f) ++ "</f>"

                withCached typeAttr v =
                    open typeAttr ++ formulaXml ++ "<v>" ++ v ++ "</v></c>"
            in
            case cached of
                Nothing ->
                    ( Just (open "" ++ formulaXml ++ "</c>"), { styledState | needsRecalc = True } )

                Just (Text s) ->
                    ( Just (withCached " t=\"str\"" (Escape.text (Escape.encodeXString s))), styledState )

                Just (Number n) ->
                    ( Just (withCached "" (number n)), styledState )

                Just (Boolean b) ->
                    ( Just (withCached " t=\"b\"" (boolValue b)), styledState )

                Just (DateValue d) ->
                    ( Just (withCached "" (String.fromInt (XDate.dateToSerial XDate.Excel1900 d))), styledState )

                Just (DateTimeValue dt) ->
                    ( Just (withCached "" (number (XDate.dateTimeToSerial XDate.Excel1900 dt))), styledState )

                Just (ErrorValue code) ->
                    ( Just (withCached " t=\"e\"" (Escape.text code)), styledState )

                Just _ ->
                    ( Just (open "" ++ formulaXml ++ "</c>"), { styledState | needsRecalc = True } )

        Blank ->
            if xf == 0 then
                ( Nothing, state )

            else
                ( Just ("<c r=\"" ++ ref ++ "\" s=\"" ++ String.fromInt xf ++ "\"/>"), styledState )


{-| Formulas are stored without the leading `=`, with newer functions
prefixed.
-}
formulaText : String -> String
formulaText f =
    let
        trimmed =
            String.trim f

        body =
            if String.startsWith "=" trimmed then
                String.dropLeft 1 trimmed

            else
                trimmed
    in
    FutureFunctions.addPrefixes body


{-| `String.fromFloat` gives the shortest text that reads back as the same
number, which is what `xsd:double` expects.
-}
number : Float -> String
number n =
    String.fromFloat n


boolValue : Bool -> String
boolValue b =
    if b then
        "1"

    else
        "0"
