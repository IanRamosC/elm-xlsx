module Xlsx.Internal.SheetReader exposing
    ( Context, RawCell, RawValue(..), SheetContent
    , read
    )

{-| Reads a worksheet part (`xl/worksheets/sheetN.xml`) into cells, with the
split-based scanner.

What it relies on, and what it tolerates:

  - The namespace prefix is taken from the root element (`<x:worksheet>`),
    as written by the .NET Open XML SDK.
  - Rows and cells without an `r` attribute follow the previous one, as
    some generators omit it.
  - `<dimension>` is ignored: it is often missing or wrong. The real extent
    is computed from the cells that have a value or a formula; cells that
    are only formatted are skipped.
  - Shared formulas: only the anchor cell carries the formula text; the
    other cells in the range get `Nothing`.

@docs Context, RawCell, RawValue, SheetContent
@docs read

-}

import Array exposing (Array)
import Xlsx.Internal.FutureFunctions as FutureFunctions
import Xlsx.Internal.StylesReader as StylesReader exposing (CellFormat)
import Xlsx.Internal.Xml.Scan as Scan
import Xlsx.Ref as Ref


{-| What the workbook provides to read a sheet.
-}
type alias Context =
    { sharedStrings : Array String
    , formats : Array CellFormat
    }


{-| A cell value as stored, before date conversion.
-}
type RawValue
    = RawText String
    | RawNumber Float
    | RawBool Bool
    | RawError String
    | RawIsoDate String
    | RawEmpty


{-| -}
type alias RawCell =
    { row : Int
    , column : Int
    , value : RawValue
    , formula : Maybe String
    , format : CellFormat
    }


{-| Cells in document order (row by row, left to right), merged ranges as
written (`"A1:B2"`), and the used extent.
-}
type alias SheetContent =
    { cells : List RawCell
    , merges : List String
    , lastRow : Int
    , lastColumn : Int
    }


{-| -}
read : Context -> String -> SheetContent
read context xml =
    let
        prefix =
            Scan.rootPrefix xml

        ( data, afterData ) =
            sheetData prefix xml

        ( cells, lastRow, lastColumn ) =
            Scan.elements prefix "row" data
                |> List.foldl (readRow context prefix) ( [], 0, ( 0, 0 ) )
                |> (\( acc, _, ( r, c ) ) -> ( List.reverse acc, r, c ))
    in
    { cells = cells
    , merges =
        Scan.elements prefix "mergeCell" afterData
            |> List.filterMap (Scan.startTag >> Scan.attribute "ref")
    , lastRow = lastRow
    , lastColumn = lastColumn
    }


{-| The content of `<sheetData>` and everything after it.
-}
sheetData : String -> String -> ( String, String )
sheetData prefix xml =
    case Scan.elements prefix "sheetData" xml of
        chunk :: _ ->
            let
                close =
                    "</" ++ prefix ++ "sheetData>"
            in
            case String.indexes close chunk of
                end :: _ ->
                    ( String.left end chunk, String.dropLeft (end + String.length close) chunk )

                [] ->
                    -- `<sheetData/>`: no rows
                    ( "", chunk )

        [] ->
            ( "", "" )


{-| Fold state: cells so far (reversed), the previous row number, and the
extent.
-}
readRow :
    Context
    -> String
    -> String
    -> ( List RawCell, Int, ( Int, Int ) )
    -> ( List RawCell, Int, ( Int, Int ) )
readRow context prefix chunk ( acc, previousRow, ( maxRow, maxColumn ) ) =
    let
        rowNumber =
            Scan.startTag chunk
                |> Scan.attribute "r"
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault (previousRow + 1)

        ( newAcc, _, rowMaxColumn ) =
            Scan.elements prefix "c" chunk
                |> List.foldl (readCell context prefix rowNumber) ( acc, 0, 0 )

        hasCells =
            rowMaxColumn > 0
    in
    ( newAcc
    , rowNumber
    , ( if hasCells then
            max maxRow rowNumber

        else
            maxRow
      , max maxColumn rowMaxColumn
      )
    )


readCell : Context -> String -> Int -> String -> ( List RawCell, Int, Int ) -> ( List RawCell, Int, Int )
readCell context prefix rowNumber chunk ( acc, previousColumn, maxColumn ) =
    let
        tag =
            Scan.startTag chunk

        column =
            Scan.attribute "r" tag
                |> Maybe.andThen Ref.fromString
                |> Maybe.map .column
                |> Maybe.withDefault (previousColumn + 1)

        body =
            cellBody chunk

        format =
            Scan.attribute "s" tag
                |> Maybe.andThen String.toInt
                |> Maybe.andThen (\i -> Array.get i context.formats)
                |> Maybe.withDefault StylesReader.general

        formula =
            Scan.content prefix "f" body
                |> Maybe.andThen
                    (\raw ->
                        if String.isEmpty raw then
                            Nothing

                        else
                            Just (FutureFunctions.removePrefixes (Scan.decodeText raw))
                    )

        value =
            rawValue context prefix (Scan.attribute "t" tag) body
    in
    if value == RawEmpty && formula == Nothing then
        -- a formatted but empty cell: nothing to read
        ( acc, column, maxColumn )

    else
        ( { row = rowNumber
          , column = column
          , value = value
          , formula = formula
          , format = format
          }
            :: acc
        , column
        , max maxColumn column
        )


{-| The part of a cell chunk after its start tag; empty for `<c .../>`.
-}
cellBody : String -> String
cellBody chunk =
    case String.indexes ">" chunk of
        i :: _ ->
            if String.slice (i - 1) i chunk == "/" then
                ""

            else
                String.dropLeft (i + 1) chunk

        [] ->
            ""


rawValue : Context -> String -> Maybe String -> String -> RawValue
rawValue context prefix cellType body =
    let
        v =
            Scan.content prefix "v" body
    in
    case cellType of
        Just "s" ->
            v
                |> Maybe.andThen (String.trim >> String.toInt)
                |> Maybe.andThen (\i -> Array.get i context.sharedStrings)
                |> Maybe.map RawText
                |> Maybe.withDefault RawEmpty

        Just "str" ->
            RawText (Maybe.withDefault "" (Maybe.map Scan.decodeText v))

        Just "inlineStr" ->
            case Scan.elements prefix "is" body of
                is :: _ ->
                    RawText (Scan.textRuns prefix is)

                [] ->
                    -- some writers put inline text in <v>
                    v |> Maybe.map (Scan.decodeText >> RawText) |> Maybe.withDefault RawEmpty

        Just "b" ->
            case Maybe.map String.trim v of
                Just "1" ->
                    RawBool True

                Just "true" ->
                    RawBool True

                Just "0" ->
                    RawBool False

                Just "false" ->
                    RawBool False

                _ ->
                    RawEmpty

        Just "e" ->
            v |> Maybe.map (Scan.decodeText >> RawError) |> Maybe.withDefault RawEmpty

        Just "d" ->
            v |> Maybe.map (String.trim >> RawIsoDate) |> Maybe.withDefault RawEmpty

        _ ->
            v
                |> Maybe.andThen (String.trim >> String.toFloat)
                |> Maybe.map RawNumber
                |> Maybe.withDefault RawEmpty
