# elm-xlsx

Read and write Excel `.xlsx` files in pure Elm. Bytes go in and bytes come out, with no ports and no JavaScript.

- **Write** workbooks with several sheets, styles, number formats, dates, formulas, hyperlinks, merged cells, frozen panes and filters.
- **Export** a list of records as a ready-made table with one function.
- **Read** files saved by Excel and the common libraries (openpyxl, XlsxWriter, the .NET Open XML SDK), including ISO 29500 Strict files.
- **Decode** rows into your own types, the way `Json.Decode` works, with errors that name the row and column.

```sh
elm install ianramosc/elm-xlsx
```

## Export a table

```elm
import Xlsx.Encode as Encode
import Xlsx.Style as Style
import Xlsx.Write as Write


type alias Order =
    { id : Int, customer : String, total : Float, placed : Date }


ordersFile : Time.Posix -> List Order -> Result (List Write.Problem) Bytes
ordersFile now orders =
    Write.workbook
        [ Encode.table "Orders"
            [ Encode.int "Order" .id
            , Encode.string "Customer" .customer
            , Encode.float "Total" .total
                |> Encode.style (Style.none |> Style.numberFormat (Style.currency "$" 2))
            , Encode.date "Placed" .placed
            ]
            orders
        ]
        |> Write.toBytes { created = now, compression = Write.Auto }
```

`Encode.table` makes the header row bold, freezes it so it stays visible while scrolling, adds filter buttons and fits the column widths to the content.

To let the user save the file, use [`elm/file`](https://package.elm-lang.org/packages/elm/file/latest/):

```elm
File.Download.bytes "orders.xlsx"
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    bytes
```

## Build a workbook by hand

```elm
import Xlsx.Style as Style
import Xlsx.Write as Write


header : Style.Style
header =
    Style.none
        |> Style.bold
        |> Style.fill (Style.rgb 221 235 247)
        |> Style.borderBottom Style.Thin Style.black


report : Write.Workbook
report =
    Write.workbook
        [ Write.sheet "Summary"
            [ Write.row (List.map (Write.string >> Write.withStyle header) [ "Region", "Sales" ])
            , Write.row [ Write.string "North", Write.float 1250.5 ]
            , Write.row [ Write.string "South", Write.float 980 ]
            , Write.emptyRow
            , Write.row
                [ Write.string "Total"
                , Write.formula "SUM(B2:B3)"
                ]
            , Write.row [ Write.hyperlink "https://example.com/report" "Full report" ]
            ]
            |> Write.columnWidths [ 18, 12 ]
            |> Write.freeze { rows = 1, columns = 0 }
        ]
        |> Write.withProperties [ Write.title "Sales", Write.author "Finance" ]
```

`Write.toBytes` checks the whole workbook before writing it. If anything is wrong, such as a sheet name Excel refuses, a `NaN`, a date before 1900 or a string longer than 32,767 characters, it returns every problem at once, each with its sheet and cell:

```elm
case Write.toBytes Write.defaultOptions report of
    Ok bytes ->
        download bytes

    Err problems ->
        showErrors (List.map Write.problemToString problems)
```

Rows are dense: each row lists its cells from column A, and each sheet lists its rows from row 1. To start further in, use `Write.rowFrom`, `Write.emptyRow` and `Write.emptyRows`.

## Read a file

```elm
import File exposing (File)
import Task
import Xlsx.Read as Read exposing (CellValue(..))


type Msg
    = FileChosen File
    | FileLoaded Bytes


update msg model =
    case msg of
        FileChosen file ->
            ( model, Task.perform FileLoaded (File.toBytes file) )

        FileLoaded bytes ->
            case Read.read bytes |> Result.andThen Read.firstSheet of
                Ok sheet ->
                    ( { model | rows = Read.rows sheet }, Cmd.none )

                Err error ->
                    ( { model | error = Just (Read.errorToString error) }, Cmd.none )
```

Each cell is one of `Text`, `Number`, `Boolean`, `DateValue`, `DateTimeValue`, `ErrorValue` or `Empty`. With `Read.cell` or `Read.cells` you also get the cell's formula, its number format and its hyperlink.

When you open a file, only its structure is read. Each sheet is parsed when you ask for it with `Read.sheet`, so reading one sheet of a large workbook does not pay for the others.

## Decode rows into your types

```elm
import Xlsx.Decode as Decode exposing (Decoder)


type alias Person =
    { name : String, age : Int, email : Maybe String }


person : Decoder Person
person =
    Decode.into Person
        |> Decode.pipeline (Decode.field "Name" Decode.string)
        |> Decode.pipeline (Decode.field "Age" Decode.int)
        |> Decode.pipeline (Decode.optionalField "Email" Decode.string)


people : Read.Sheet -> Result Decode.Error (List Person)
people =
    Decode.decodeSheet Decode.FieldNamesFromFirstRow person
```

Every row is decoded, and all the failures are reported together. `Decode.errorToString` gives messages like this:

```text
Sheet "People", row 7, column C ("Age"): expected a whole number, found the text "forty"
```

## Things to know

- **Formulas are not calculated.** When reading, a formula cell's value is the result that the program which saved the file stored in it. When writing, Excel, Google Sheets, LibreOffice and Numbers recalculate formulas when they open the file. Other readers only see a cached result if you pass one with `Write.formulaWithResult`.
- **Dates are numbers with a date format**, as they are in Excel. When reading, a number is a `DateValue` or a `DateTimeValue` only if its format shows a date. Both the 1900 and the 1904 date systems are supported. `Xlsx.Date` converts between dates and Excel's serial numbers.
- **`rows` or `cells`.** `Read.rows` fills gaps with `Empty`, which is what you want for tables. For sparse sheets, such as a single cell at `ZZ100000`, use `Read.cells`, which lists only the cells that exist.
- **Limits.** Excel's own limits are enforced when writing: 1,048,576 rows, 16,384 columns and 32,767 characters per cell. When reading, files bigger than 256 MB uncompressed and parts that look like zip bombs are refused. `Read.readWith` changes these limits.
- **Not supported:** charts, images, comments, conditional formatting, data validation, rich text runs when writing, password-protected files, `.xls`, `.xlsb` and `.ods`. When a file can't be read, you get an `Error` that says why.
- **Speed.** Reading takes about half a second per 100,000 cells in Node. Writing a 200,000-cell workbook takes a few seconds. `Write.Auto` compression leaves parts over 4 MB uncompressed, so that large exports don't freeze the page.

## Example app

[`examples/`](https://github.com/ianramosc/elm-xlsx/tree/main/examples) has a small app that opens a file, shows a sheet as a table, decodes it, and exports it again.

## License

BSD-3-Clause
