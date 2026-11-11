module Xlsx.Read exposing
    ( Workbook, read
    , ReadOptions, defaultReadOptions, readWith
    , SheetInfo, Visibility(..), SheetKind(..), sheets, sheetNames
    , Sheet, sheet, firstSheet, name
    , Cell, CellValue(..), rows, cells, cell, dimensions, merges
    , Error(..), errorToString
    )

{-| Read `.xlsx` files.

    import Xlsx.Read as Read exposing (CellValue(..))

    firstColumn : Bytes -> Result Read.Error (List CellValue)
    firstColumn bytes =
        Read.read bytes
            |> Result.andThen Read.firstSheet
            |> Result.map (Read.rows >> List.filterMap List.head)

Opening a file reads its directory, workbook, shared strings and styles.
Each sheet is parsed only when you ask for it with [`sheet`](#sheet), so
reading one sheet of a large workbook does not pay for the others.

Dates are recognised from the cell's number format, as Excel does: a number
shown as `yyyy-mm-dd` is read as a [`DateValue`](#CellValue), one shown with a
time as a `DateTimeValue`. Both 1900 and 1904 date systems are handled.

Formulas are not calculated. A formula cell's value is the result cached in
the file by the program that saved it; files written by tools that do not
calculate (including [`Xlsx.Write`](Xlsx-Write) without
`formulaWithResult`) have no cached result, and the value is `Empty`.


# Workbooks

@docs Workbook, read
@docs ReadOptions, defaultReadOptions, readWith
@docs SheetInfo, Visibility, SheetKind, sheets, sheetNames


# Opening a sheet

@docs Sheet, sheet, firstSheet, name


# Cell values

@docs Cell, CellValue, rows, cells, cell, dimensions, merges


# Errors

@docs Error, errorToString

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Date exposing (Date)
import Dict exposing (Dict)
import Xlsx.Date as XDate
import Xlsx.Internal.Archive as Archive exposing (Archive)
import Xlsx.Internal.NumFmt as NumFmt
import Xlsx.Internal.Opc as Opc
import Xlsx.Internal.SharedStrings as SharedStrings
import Xlsx.Internal.SheetReader as SheetReader exposing (RawValue(..))
import Xlsx.Internal.StylesReader as StylesReader exposing (CellFormat)
import Xlsx.Internal.Xml.Scan as Scan
import Xlsx.Ref as Ref exposing (Range, Ref)



-- WORKBOOK


{-| An opened `.xlsx` file.
-}
type Workbook
    = Workbook
        { archive : Archive
        , sheets : List SheetEntry
        , dateSystem : XDate.DateSystem
        , sharedStrings : Array String
        , formats : Array CellFormat
        }


type alias SheetEntry =
    { info : SheetInfo
    , part : Maybe String
    }


{-| Limits that protect against hostile or broken files.

  - `maxUncompressedBytes`: refuse files whose parts would take more than
    this once decompressed.
  - `minCompressionRatio`: refuse parts of 1 MB or more that compress better
    than this (compressed size ÷ uncompressed size). Ordinary spreadsheets
    compress to 5–20%; zip bombs to far less than 1%.

-}
type alias ReadOptions =
    { maxUncompressedBytes : Int
    , minCompressionRatio : Float
    }


{-| 256 MB uncompressed, and a minimum ratio of 0.01 (100:1).
-}
defaultReadOptions : ReadOptions
defaultReadOptions =
    { maxUncompressedBytes = 256 * 1024 * 1024
    , minCompressionRatio = 0.01
    }


{-| Open a file with the [default limits](#defaultReadOptions).
-}
read : Bytes -> Result Error Workbook
read =
    readWith defaultReadOptions


{-| Open a file with your own limits.
-}
readWith : ReadOptions -> Bytes -> Result Error Workbook
readWith options bytes =
    Archive.open { maxUncompressedBytes = options.maxUncompressedBytes, minRatio = options.minCompressionRatio } bytes
        |> Result.mapError archiveError
        |> Result.andThen openWorkbook


archiveError : Archive.ArchiveError -> Error
archiveError err =
    case err of
        Archive.NotAZip ->
            NotAnXlsxFile

        Archive.OleCompoundFile ->
            EncryptedOrLegacyXls

        Archive.TooLarge message ->
            LimitExceeded message


openWorkbook : Archive -> Result Error Workbook
openWorkbook archive =
    if Archive.has "xl/workbook.bin" archive then
        Err (UnsupportedFormat "This is a binary workbook (.xlsb). Save it as .xlsx to read it.")

    else if Archive.has "content.xml" archive && Archive.has "mimetype" archive then
        Err (UnsupportedFormat "This is an OpenDocument spreadsheet (.ods). Save it as .xlsx to read it.")

    else
        workbookPath archive
            |> Result.andThen
                (\path ->
                    Result.map2 Tuple.pair
                        (readWorkbookXml path archive)
                        (readRelationships path archive)
                        |> Result.andThen (buildWorkbook archive path)
                )


{-| The main part, from the package relationships; `xl/workbook.xml` when
those are missing.
-}
workbookPath : Archive -> Result Error String
workbookPath archive =
    let
        fallback =
            if Archive.has "xl/workbook.xml" archive then
                Ok "xl/workbook.xml"

            else
                Err NotAnXlsxFile
    in
    case readRelationships "" archive of
        Ok rels ->
            case List.filter (.type_ >> Opc.isRelType "officeDocument") rels of
                rel :: _ ->
                    let
                        path =
                            Opc.resolveTarget "" rel.target
                    in
                    if Archive.has path archive then
                        Ok path

                    else
                        fallback

                [] ->
                    fallback

        Err _ ->
            fallback


buildWorkbook : Archive -> String -> ( ( String, String ), List Opc.Relationship ) -> Result Error Workbook
buildWorkbook archive path ( ( prefix, xml ), rels ) =
    let
        -- the related part, or the usual file next to the workbook when the
        -- relationship is missing (as calamine does)
        target kind defaultName =
            rels
                |> List.filter (\r -> Opc.isRelType kind r.type_ && not r.external)
                |> List.head
                |> Maybe.map (.target >> Opc.resolveTarget path)
                |> Maybe.withDefault (Opc.resolveTarget path defaultName)
    in
    Result.map2
        (\strings formats ->
            let
                byId =
                    Dict.fromList (List.map (\r -> ( r.id, r )) rels)

                date1904 =
                    Scan.elements prefix "workbookPr" xml
                        |> List.head
                        |> Maybe.andThen (Scan.startTag >> Scan.attribute "date1904")
                        |> Maybe.map (\v -> v == "1" || String.toLower v == "true")
                        |> Maybe.withDefault False

                entries =
                    Scan.section prefix "sheets" xml
                        |> Maybe.withDefault ""
                        |> Scan.elements prefix "sheet"
                        |> List.filterMap (Scan.startTag >> sheetEntry path byId)
            in
            Workbook
                { archive = archive
                , sheets = entries
                , dateSystem =
                    if date1904 then
                        XDate.Excel1904

                    else
                        XDate.Excel1900
                , sharedStrings = strings
                , formats = formats
                }
        )
        (optionalPart (target "sharedStrings" "sharedStrings.xml") archive SharedStrings.parse Array.empty)
        (optionalPart (target "styles" "styles.xml") archive StylesReader.parse Array.empty)


sheetEntry : String -> Dict String Opc.Relationship -> String -> Maybe SheetEntry
sheetEntry workbookPart byId tag =
    Scan.attribute "name" tag
        |> Maybe.map
            (\sheetName ->
                let
                    rel =
                        Scan.prefixedAttribute "id" tag |> Maybe.andThen (\id -> Dict.get id byId)

                    kind =
                        case Maybe.map .type_ rel of
                            Just t ->
                                if Opc.isRelType "chartsheet" t then
                                    Chartsheet

                                else if Opc.isRelType "dialogsheet" t then
                                    DialogSheet

                                else if Opc.isRelType "xlMacrosheet" t || Opc.isRelType "xlIntlMacrosheet" t then
                                    MacroSheet

                                else
                                    Worksheet

                            Nothing ->
                                Worksheet
                in
                { info =
                    { name = sheetName
                    , visibility =
                        case Scan.attribute "state" tag of
                            Just "hidden" ->
                                Hidden

                            Just "veryHidden" ->
                                VeryHidden

                            _ ->
                                Visible
                    , kind = kind
                    }
                , part = Maybe.map (.target >> Opc.resolveTarget workbookPart) rel
                }
            )


optionalPart : String -> Archive -> (String -> Result String a) -> a -> Result Error a
optionalPart path archive parse default =
    case readText path archive of
        Err (MissingPart _) ->
            Ok default

        Err e ->
            Err e

        Ok text ->
            parse text |> Result.mapError (\problem -> InvalidXml { part = path, problem = problem })


readText : String -> Archive -> Result Error String
readText path archive =
    Archive.readText path archive
        |> Result.mapError
            (\err ->
                case err of
                    Archive.PartMissing ->
                        MissingPart path

                    Archive.PartCorrupt problem ->
                        InvalidXml { part = path, problem = problem }

                    Archive.PartUtf16 ->
                        UnsupportedEncoding path
            )


{-| The workbook part and its namespace prefix.
-}
readWorkbookXml : String -> Archive -> Result Error ( String, String )
readWorkbookXml path archive =
    readText path archive
        |> Result.andThen
            (\xml ->
                Scan.document "workbook" xml
                    |> Result.map (\prefix -> ( prefix, xml ))
                    |> Result.mapError (\problem -> InvalidXml { part = path, problem = problem })
            )


readRelationships : String -> Archive -> Result Error (List Opc.Relationship)
readRelationships part archive =
    let
        path =
            Opc.relsPathFor part
    in
    case readText path archive of
        Err (MissingPart _) ->
            Ok []

        Err e ->
            Err e

        Ok text ->
            Opc.parseRelationships text
                |> Result.mapError (\problem -> InvalidXml { part = path, problem = problem })



-- SHEET LIST


{-| A sheet as listed in the workbook, in tab order.
-}
type alias SheetInfo =
    { name : String
    , visibility : Visibility
    , kind : SheetKind
    }


{-| `VeryHidden` sheets can only be shown again from the VBA editor.
-}
type Visibility
    = Visible
    | Hidden
    | VeryHidden


{-| Only worksheets hold cells. Chart sheets, dialog sheets and Excel 4 macro
sheets are listed so you can see them, but [`sheet`](#sheet) refuses them.
-}
type SheetKind
    = Worksheet
    | Chartsheet
    | DialogSheet
    | MacroSheet


{-| All sheets, in tab order.
-}
sheets : Workbook -> List SheetInfo
sheets (Workbook wb) =
    List.map .info wb.sheets


{-| The names of the worksheets, in tab order, hidden ones included.
-}
sheetNames : Workbook -> List String
sheetNames (Workbook wb) =
    wb.sheets
        |> List.filter (\e -> e.info.kind == Worksheet)
        |> List.map (.info >> .name)



-- SHEET


{-| A parsed worksheet.
-}
type Sheet
    = Sheet
        { name : String
        , rows : Dict Int (Dict Int Cell)
        , lastRow : Int
        , lastColumn : Int
        , merges : List Range
        }


{-| A worksheet by name. Names are matched ignoring case, as Excel does.
-}
sheet : String -> Workbook -> Result Error Sheet
sheet sheetName ((Workbook wb) as workbook) =
    case List.filter (\e -> String.toLower e.info.name == String.toLower sheetName) wb.sheets of
        entry :: _ ->
            parseSheet workbook entry

        [] ->
            Err (SheetNotFound sheetName)


{-| The first worksheet in tab order, hidden or not.
-}
firstSheet : Workbook -> Result Error Sheet
firstSheet ((Workbook wb) as workbook) =
    case List.filter (\e -> e.info.kind == Worksheet) wb.sheets of
        entry :: _ ->
            parseSheet workbook entry

        [] ->
            Err (SheetNotFound "(the workbook has no worksheets)")


parseSheet : Workbook -> SheetEntry -> Result Error Sheet
parseSheet (Workbook wb) entry =
    case ( entry.info.kind, entry.part ) of
        ( Worksheet, Just path ) ->
            readText path wb.archive
                |> Result.andThen
                    (\xml ->
                        if Scan.hasDoctype xml then
                            Err (InvalidXml { part = path, problem = "it has a DOCTYPE, which spreadsheets never use" })

                        else
                            let
                                content =
                                    SheetReader.read { sharedStrings = wb.sharedStrings, formats = wb.formats } xml

                                rels =
                                    if List.any (\l -> l.relationship /= Nothing) content.hyperlinks then
                                        readRelationships path wb.archive

                                    else
                                        Ok []
                            in
                            rels
                                |> Result.map
                                    (\relationships ->
                                        Sheet
                                            { name = entry.info.name
                                            , rows =
                                                List.foldl (insertCell wb.dateSystem) Dict.empty content.cells
                                                    |> addLinks relationships content.hyperlinks
                                            , lastRow = content.lastRow
                                            , lastColumn = content.lastColumn
                                            , merges = List.filterMap Ref.rangeFromString content.merges
                                            }
                                    )
                    )

        ( Worksheet, Nothing ) ->
            Err (MissingPart ("the part for sheet " ++ entry.info.name))

        _ ->
            Err (UnsupportedFormat ("\"" ++ entry.info.name ++ "\" is not a worksheet, so it has no cells."))


{-| Set the `hyperlink` of every cell a `<hyperlink>` covers. External
targets come from the sheet's relationships; a location alone becomes
`#location`.
-}
addLinks : List Opc.Relationship -> List SheetReader.RawLink -> Dict Int (Dict Int Cell) -> Dict Int (Dict Int Cell)
addLinks relationships links cellRows =
    let
        targets =
            Dict.fromList (List.map (\r -> ( r.id, r.target )) relationships)

        linkText link =
            case ( link.relationship |> Maybe.andThen (\id -> Dict.get id targets), link.location ) of
                ( Just url, Just location ) ->
                    Just (url ++ "#" ++ location)

                ( Just url, Nothing ) ->
                    Just url

                ( Nothing, Just location ) ->
                    Just ("#" ++ location)

                ( Nothing, Nothing ) ->
                    Nothing
    in
    List.foldl
        (\link acc ->
            case ( Ref.rangeFromString link.ref, linkText link ) of
                ( Just range, Just text ) ->
                    Dict.map
                        (\r columns ->
                            if r >= range.start.row && r <= range.end.row then
                                Dict.map
                                    (\c cell_ ->
                                        if c >= range.start.column && c <= range.end.column then
                                            { cell_ | hyperlink = Just text }

                                        else
                                            cell_
                                    )
                                    columns

                            else
                                columns
                        )
                        acc

                _ ->
                    acc
        )
        cellRows
        links


insertCell : XDate.DateSystem -> SheetReader.RawCell -> Dict Int (Dict Int Cell) -> Dict Int (Dict Int Cell)
insertCell system raw acc =
    let
        c =
            { value = toValue system raw.format raw.value
            , formula = raw.formula
            , numberFormat = raw.format.code
            , hyperlink = Nothing
            }
    in
    Dict.update raw.row
        (\existing -> Just (Dict.insert raw.column c (Maybe.withDefault Dict.empty existing)))
        acc


{-| The sheet's name.
-}
name : Sheet -> String
name (Sheet s) =
    s.name



-- CELLS


{-| A cell: its value, the formula that produced it (without the leading
`=`), its number format code (`"General"`, `"0.00%"`, `"yyyy-mm-dd"`, …),
and where it links to.

`hyperlink` is a URL (`"https://example.com"`), or `#` followed by a place in
the workbook (`"#'Sheet 2'!A1"`), as in Excel's `HYPERLINK` function.

-}
type alias Cell =
    { value : CellValue
    , formula : Maybe String
    , numberFormat : String
    , hyperlink : Maybe String
    }


{-| A cell value.

  - `Number` also covers numbers formatted as durations (`[h]:mm:ss`): their
    value is a count of days.
  - `ErrorValue` holds the error code, such as `"#N/A"` or `"#DIV/0!"`.
  - `Empty` is a cell with nothing in it, or a formula with no cached result.

-}
type CellValue
    = Text String
    | Number Float
    | Boolean Bool
    | DateValue Date
    | DateTimeValue XDate.DateTime
    | ErrorValue String
    | Empty


emptyCell : Cell
emptyCell =
    { value = Empty, formula = Nothing, numberFormat = "General", hyperlink = Nothing }


toValue : XDate.DateSystem -> CellFormat -> RawValue -> CellValue
toValue system format raw =
    case raw of
        RawText s ->
            Text s

        RawBool b ->
            Boolean b

        RawError e ->
            ErrorValue e

        RawEmpty ->
            Empty

        RawIsoDate iso ->
            parseIsoDate iso |> Maybe.withDefault (Text iso)

        RawNumber n ->
            case format.kind of
                NumFmt.Date ->
                    if n == toFloat (floor n) then
                        XDate.serialToDate system n |> Maybe.map DateValue |> Maybe.withDefault (Number n)

                    else
                        XDate.serialToDateTime system n |> Maybe.map DateTimeValue |> Maybe.withDefault (Number n)

                NumFmt.DateTime ->
                    XDate.serialToDateTime system n |> Maybe.map DateTimeValue |> Maybe.withDefault (Number n)

                _ ->
                    Number n


{-| `t="d"` cells hold ISO 8601 text: `2024-02-29` or `2024-02-29T13:45:00`
(optionally with fractional seconds and a `Z`).
-}
parseIsoDate : String -> Maybe CellValue
parseIsoDate iso =
    case String.split "T" iso of
        [ day ] ->
            Date.fromIsoString day |> Result.toMaybe |> Maybe.map DateValue

        [ day, time ] ->
            Maybe.map2
                (\d ( h, m, ms ) ->
                    if h == 0 && m == 0 && ms == 0 then
                        DateValue d

                    else
                        DateTimeValue
                            { date = d
                            , hour = h
                            , minute = m
                            , second = ms // 1000
                            , millisecond = modBy 1000 ms
                            }
                )
                (Date.fromIsoString day |> Result.toMaybe)
                (parseTime time)

        _ ->
            Nothing


parseTime : String -> Maybe ( Int, Int, Int )
parseTime time =
    let
        clock =
            time
                |> String.split "Z"
                |> List.head
                |> Maybe.withDefault time
                |> String.split "+"
                |> List.head
                |> Maybe.withDefault time
    in
    case String.split ":" clock of
        [ h, m ] ->
            Maybe.map2 (\hh mm -> ( hh, mm, 0 )) (String.toInt h) (String.toInt m)

        [ h, m, s ] ->
            Maybe.map3 (\hh mm ss -> ( hh, mm, round (ss * 1000) )) (String.toInt h) (String.toInt m) (String.toFloat s)

        _ ->
            Nothing


{-| The sheet as rows of values, from row 1 to the last row with data. Each
row starts at column A and runs to its own last cell, so rows can have
different lengths; gaps (and rows with no data) are filled with `Empty`.

    -- A1 = "Name", B1 = "Age", A2 = "Ada", B2 = 36
    rows sheet == [ [ Text "Name", Text "Age" ], [ Text "Ada", Number 36 ] ]

-}
rows : Sheet -> List (List CellValue)
rows (Sheet s) =
    List.range 1 s.lastRow
        |> List.map
            (\r ->
                case Dict.get r s.rows of
                    Just columns ->
                        let
                            lastColumn =
                                Dict.foldl (\c _ _ -> c) 0 columns
                        in
                        List.range 1 lastColumn
                            |> List.map (\c -> Dict.get c columns |> Maybe.map .value |> Maybe.withDefault Empty)

                    Nothing ->
                        []
            )


{-| Every cell that has a value or a formula, row by row, left to right.
-}
cells : Sheet -> List ( Ref, Cell )
cells (Sheet s) =
    Dict.foldr
        (\r columns acc ->
            Dict.foldr (\c value inner -> ( { column = c, row = r }, value ) :: inner) acc columns
        )
        []
        s.rows


{-| The cell at a reference. Cells with nothing in them are `Empty`.

    cell { column = 2, row = 2 } sheet

    Ref.fromString "B2" |> Maybe.map (\ref -> cell ref sheet)

-}
cell : Ref -> Sheet -> Cell
cell ref (Sheet s) =
    Dict.get ref.row s.rows
        |> Maybe.andThen (Dict.get ref.column)
        |> Maybe.withDefault emptyCell


{-| The range from A1 to the last row and column with data, or `Nothing` for
an empty sheet. The `<dimension>` stored in the file is not trusted.
-}
dimensions : Sheet -> Maybe Range
dimensions (Sheet s) =
    if s.lastRow == 0 then
        Nothing

    else
        Just { start = { column = 1, row = 1 }, end = { column = s.lastColumn, row = s.lastRow } }


{-| The merged ranges. The value of a merged range is in its top-left cell.
-}
merges : Sheet -> List Range
merges (Sheet s) =
    s.merges



-- ERRORS


{-| Why a file could not be read.

  - `NotAnXlsxFile`: not a ZIP file, or a ZIP file without a workbook (a
    `.docx`, for example).
  - `EncryptedOrLegacyXls`: an old binary `.xls` file, or an `.xlsx` saved
    with a password. Both are OLE2 compound files.
  - `UnsupportedFormat`: a format this package cannot read, such as `.xlsb`
    or `.ods`, or a chart sheet asked for as a worksheet.
  - `UnsupportedEncoding`: the part is UTF-16, not UTF-8.
  - `MissingPart`: the file refers to a part that is not in it.
  - `InvalidXml`: a part is not valid XML. Parts that cannot be
    decompressed (a bad checksum, an unknown compression method) are
    reported this way too, with the reason in `problem`.
  - `LimitExceeded`: the file breaks one of the [`ReadOptions`](#ReadOptions)
    limits.
  - `SheetNotFound`: no sheet has that name.

-}
type Error
    = NotAnXlsxFile
    | EncryptedOrLegacyXls
    | UnsupportedFormat String
    | UnsupportedEncoding String
    | MissingPart String
    | InvalidXml { part : String, problem : String }
    | LimitExceeded String
    | SheetNotFound String


{-| A message you can show to people.
-}
errorToString : Error -> String
errorToString error =
    case error of
        NotAnXlsxFile ->
            "This is not an Excel (.xlsx) file."

        EncryptedOrLegacyXls ->
            "This is either an old Excel file (.xls) or a password-protected workbook. Open it in Excel and save it as .xlsx without a password."

        UnsupportedFormat message ->
            message

        UnsupportedEncoding part ->
            "The part " ++ part ++ " is not UTF-8 text."

        MissingPart part ->
            "The file is incomplete: " ++ part ++ " is missing."

        InvalidXml { part, problem } ->
            "The file is damaged: " ++ part ++ " could not be read (" ++ problem ++ ")."

        LimitExceeded message ->
            message

        SheetNotFound sheetName ->
            "There is no sheet named \"" ++ sheetName ++ "\"."
