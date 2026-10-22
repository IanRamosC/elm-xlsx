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

import Xlsx.Date as XDate
import Xlsx.Internal.FutureFunctions as FutureFunctions
import Xlsx.Internal.Limits as Limits
import Xlsx.Internal.Model exposing (Cell(..), Problem, Row(..), SheetData, Style, Value(..))
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


type alias Acc =
    { rowNumber : Int
    , out : List String
    , extent : Maybe Ref.Range
    , rowLimitReported : Bool
    , state : State
    }


{-| The worksheet XML. `selected` marks the active sheet's tab.
-}
write : { selected : Bool } -> SheetData -> State -> ( String, State )
write { selected } sheet state =
    let
        acc =
            List.foldl (writeRow sheet.name)
                { rowNumber = 1, out = [], extent = Nothing, rowLimitReported = False, state = state }
                sheet.rows

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
        , "<sheetViews><sheetView "
            ++ (if selected then
                    "tabSelected=\"1\" "

                else
                    ""
               )
            ++ "workbookViewId=\"0\"/></sheetViews>"
        , "<sheetFormatPr defaultRowHeight=\"15\"/>"
        , sheetData
        , "<pageMargins left=\"0.7\" right=\"0.7\" top=\"0.75\" bottom=\"0.75\" header=\"0.3\" footer=\"0.3\"/>"
        , "</worksheet>"
        ]
    , acc.state
    )


writeRow : String -> Row -> Acc -> Acc
writeRow sheetName row acc =
    case row of
        EmptyRows n ->
            { acc | rowNumber = acc.rowNumber + max 0 n }

        Row data ->
            if acc.rowNumber > Limits.maxRows then
                if acc.rowLimitReported then
                    acc

                else
                    { acc
                        | rowLimitReported = True
                        , state = addProblem { sheet = Just sheetName, cell = Nothing, message = "The sheet has more than " ++ String.fromInt Limits.maxRows ++ " rows, Excel's limit." } acc.state
                    }

            else if data.startColumn < 1 then
                { acc
                    | rowNumber = acc.rowNumber + 1
                    , state = addProblem { sheet = Just sheetName, cell = Nothing, message = "Row " ++ String.fromInt acc.rowNumber ++ " starts at column " ++ String.fromInt data.startColumn ++ "; columns start at 1." } acc.state
                }

            else
                let
                    rowAcc =
                        List.foldl (writeCell sheetName acc.rowNumber)
                            { column = data.startColumn, cells = [], first = Nothing, last = Nothing, columnLimitReported = False, state = acc.state }
                            data.cells
                in
                case ( rowAcc.first, rowAcc.last ) of
                    ( Just first, Just last ) ->
                        { acc
                            | rowNumber = acc.rowNumber + 1
                            , out = "</row>" :: rowAcc.cells ++ (rowOpen acc.rowNumber first last :: acc.out)
                            , extent = Just (grow acc.extent { column = first, row = acc.rowNumber } { column = last, row = acc.rowNumber })
                            , state = rowAcc.state
                        }

                    _ ->
                        { acc | rowNumber = acc.rowNumber + 1, state = rowAcc.state }


{-| `spans` is an optional hint Excel writes to speed up loading.
-}
rowOpen : Int -> Int -> Int -> String
rowOpen rowNumber first last =
    "<row r=\"" ++ String.fromInt rowNumber ++ "\" spans=\"" ++ String.fromInt first ++ ":" ++ String.fromInt last ++ "\">"


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


writeCell : String -> Int -> Cell -> RowAcc -> RowAcc
writeCell sheetName rowNumber (Cell cell) acc =
    if acc.column > Limits.maxColumns then
        if acc.columnLimitReported then
            acc

        else
            { acc
                | columnLimitReported = True
                , state = addProblem { sheet = Just sheetName, cell = Nothing, message = "Row " ++ String.fromInt rowNumber ++ " has cells past column XFD, Excel's last column." } acc.state
            }

    else
        let
            ref =
                Ref.toString { column = acc.column, row = rowNumber }

            problems =
                Limits.checkCell sheetName ref cell.value

            state =
                List.foldl addProblem acc.state problems
        in
        case cellXml ref cell.value (effectiveStyle cell.value cell.style) state of
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
effectiveStyle : Value -> Style -> Style
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


cellXml : String -> Value -> Style -> State -> ( Maybe String, State )
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
