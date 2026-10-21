module Xlsx.Write exposing
    ( Workbook, workbook, withActiveSheet
    , Sheet, sheet
    , Row, row, rowFrom, emptyRow, emptyRows
    , Cell, string, float, int, bool, date, dateTime, blank
    , formula, formulaWithResult, error
    , toBytes, Options, defaultOptions, Compression(..)
    , Problem, problemToString
    )

{-| Build an Excel workbook and turn it into `.xlsx` bytes.

    import Date
    import Time exposing (Month(..))
    import Xlsx.Write as Write

    bytes =
        Write.workbook
            [ Write.sheet "People"
                [ Write.row [ Write.string "Name", Write.string "Born" ]
                , Write.row
                    [ Write.string "Ada Lovelace"
                    , Write.date (Date.fromCalendarDate 1815 Dec 10)
                    ]
                ]
            ]
            |> Write.toBytes Write.defaultOptions

Download the result with
[`File.Download.bytes`](https://package.elm-lang.org/packages/elm/file/latest/File-Download#bytes)
using the MIME type
`application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`.


# Workbooks

@docs Workbook, workbook, withActiveSheet


# Sheets

@docs Sheet, sheet


# Rows

Rows are laid out top to bottom, starting at row 1. Each row's cells are laid
out left to right, starting at column A (or the column given to `rowFrom`).

@docs Row, row, rowFrom, emptyRow, emptyRows


# Cells

@docs Cell, string, float, int, bool, date, dateTime, blank


## Formulas and errors

@docs formula, formulaWithResult, error


# Writing

@docs toBytes, Options, defaultOptions, Compression
@docs Problem, problemToString

-}

import Bytes exposing (Bytes)
import Date exposing (Date)
import Time
import Xlsx.Date exposing (DateTime)
import Xlsx.Internal.Model as Model exposing (Cell(..), Row(..), Sheet(..), Value(..), Workbook(..))
import Xlsx.Internal.Package as Package



-- WORKBOOKS


{-| A workbook: one or more sheets.
-}
type alias Workbook =
    Model.Workbook


{-| A workbook with these sheets, in tab order. The first sheet is active
(shown when the file opens) unless you pick another with `withActiveSheet`.
-}
workbook : List Sheet -> Workbook
workbook sheets =
    Workbook { sheets = sheets, activeSheet = Nothing }


{-| Choose the sheet shown when the file opens, by name.
-}
withActiveSheet : String -> Workbook -> Workbook
withActiveSheet name (Workbook w) =
    Workbook { w | activeSheet = Just name }



-- SHEETS


{-| A worksheet.
-}
type alias Sheet =
    Model.Sheet


{-| A sheet with a name and its rows.

Excel's rules for names: 1 to 31 characters, none of `: \ / ? * [ ]`, not
starting or ending with `'`, and unique within the workbook (ignoring case).
`toBytes` reports names that break these rules.

-}
sheet : String -> List Row -> Sheet
sheet name rows =
    Sheet { name = name, rows = rows }



-- ROWS


{-| A row of cells, or a gap of empty rows.
-}
type alias Row =
    Model.Row


{-| A row whose cells start at column A.
-}
row : List Cell -> Row
row cells =
    Row { startColumn = 1, cells = cells }


{-| A row whose first cell goes in the given column (1 is A, 3 is C).

    -- "Total" in C5
    Write.rowFrom 3 [ Write.string "Total", Write.float 1250 ]

-}
rowFrom : Int -> List Cell -> Row
rowFrom startColumn cells =
    Row { startColumn = startColumn, cells = cells }


{-| A row with nothing in it.
-}
emptyRow : Row
emptyRow =
    EmptyRows 1


{-| Several empty rows.

    Write.sheet "Report"
        [ Write.row [ Write.string "Quarterly report" ]
        , Write.emptyRows 2
        , Write.row [ Write.string "Region", Write.string "Sales" ] -- row 4
        ]

-}
emptyRows : Int -> Row
emptyRows n =
    EmptyRows n



-- CELLS


{-| A cell value.
-}
type alias Cell =
    Model.Cell


cell : Value -> Cell
cell value =
    Cell { value = value, style = Model.defaultStyle }


{-| Text. Up to 32,767 characters.

Line breaks (`\n`) are kept, but Excel only shows them on separate lines when
the cell's style turns on text wrapping.

-}
string : String -> Cell
string s =
    cell (Text s)


{-| A number. NaN and infinity cannot be stored, and `toBytes` reports them.
-}
float : Float -> Cell
float n =
    cell (Number n)


{-| A whole number. Excel stores every number as a 64-bit float, so integers
beyond ±2^53 lose precision there.
-}
int : Int -> Cell
int n =
    cell (Number (toFloat n))


{-| `TRUE` or `FALSE`.
-}
bool : Bool -> Cell
bool b =
    cell (Boolean b)


{-| A calendar date, shown as `yyyy-mm-dd` unless the cell has another number
format. Excel supports dates from 1900-01-01 to 9999-12-31.
-}
date : Date -> Cell
date d =
    cell (DateValue d)


{-| A date with a time of day, shown as `yyyy-mm-dd hh:mm:ss` unless the cell
has another number format. Build one from a `Time.Posix` with
[`Xlsx.Date.fromPosix`](Xlsx-Date#fromPosix).
-}
dateTime : DateTime -> Cell
dateTime dt =
    cell (DateTimeValue dt)


{-| An empty cell. Use it to skip a column within a row.
-}
blank : Cell
blank =
    cell Blank


{-| A formula, with or without the leading `=`.

    Write.formula "SUM(B2:B10)"

Write formulas the way Excel stores them: English function names, commas
between arguments, and `A1` references. Newer functions such as `XLOOKUP` are
prefixed automatically so Excel recognizes them.

Excel computes the result when it opens the file. Until then, programs that do
not calculate formulas (and `Xlsx.Read`) see no value. Use `formulaWithResult`
to store the result as well.

Dynamic-array formulas that spill into neighboring cells (`FILTER`, `UNIQUE`,
`SORT`, ...) are not supported yet: they compute only their first value.

-}
formula : String -> Cell
formula f =
    cell (Formula f Nothing)


{-| A formula together with its current result, for programs that read the
file without calculating it. The result is a `string`, `float`, `int`,
`bool`, `date`, `dateTime` or `error` cell; anything else is ignored.

    Write.formulaWithResult "SUM(B2:B3)" (Write.float 30)

-}
formulaWithResult : String -> Cell -> Cell
formulaWithResult f (Cell result) =
    case result.value of
        Formula _ _ ->
            formula f

        Blank ->
            formula f

        value ->
            cell (Formula f (Just value))


{-| An error value: one of `#NULL!`, `#DIV/0!`, `#VALUE!`, `#REF!`, `#NAME?`,
`#NUM!` or `#N/A`. `toBytes` reports anything else.

    Write.error "#N/A"

-}
error : String -> Cell
error code =
    cell (ErrorValue code)



-- WRITING


{-| How to write the file.

  - `created`: the creation time stored in the document properties and the ZIP
    entries. Pass the current time (from `Time.now`) for real exports. The
    package never reads the clock itself, so the same input always gives the
    same bytes.
  - `compression`: see `Compression`.

-}
type alias Options =
    { created : Time.Posix
    , compression : Compression
    }


{-| `Auto` compression and a fixed creation time of 1980-01-01, the earliest
time a ZIP file can store.
-}
defaultOptions : Options
defaultOptions =
    { created = Time.millisToPosix 315532800000
    , compression = Auto
    }


{-| How the parts inside the `.xlsx` (a ZIP file) are compressed.

  - `Auto`: compress parts smaller than 4 MB and store bigger ones as they
    are. Compression in pure Elm runs at about 2 MB per second, so this
    keeps big exports from freezing the page.
  - `Deflate`: always compress. Smallest files, slowest.
  - `Store`: never compress. Fastest, and the file is about 5 times bigger.

All three produce valid files.

-}
type Compression
    = Auto
    | Deflate
    | Store


{-| A reason the workbook cannot be written, with where it happened when that
is known.
-}
type alias Problem =
    { sheet : Maybe String
    , cell : Maybe String
    , message : String
    }


{-| A readable description of a problem.

    problemToString { sheet = Just "Data", cell = Just "B3", message = "NaN cannot be stored in a cell." }
    --> "Data!B3: NaN cannot be stored in a cell."

-}
problemToString : Problem -> String
problemToString problem =
    case ( problem.sheet, problem.cell ) of
        ( Just s, Just c ) ->
            s ++ "!" ++ c ++ ": " ++ problem.message

        ( Just s, Nothing ) ->
            "Sheet \"" ++ s ++ "\": " ++ problem.message

        ( Nothing, _ ) ->
            problem.message


{-| Write the workbook as `.xlsx` bytes. Before writing, the workbook is
checked against Excel's limits (sheet names, sizes, numbers, dates, ...), and
every problem found is returned at once.
-}
toBytes : Options -> Workbook -> Result (List Problem) Bytes
toBytes options (Workbook w) =
    Package.write
        { created = options.created
        , compression =
            case options.compression of
                Auto ->
                    Package.Auto (4 * 1024 * 1024)

                Deflate ->
                    Package.Deflate

                Store ->
                    Package.Store
        }
        w
