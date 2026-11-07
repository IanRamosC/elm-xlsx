# Changelog

All notable changes to this package are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and the package uses
[Semantic Versioning](https://semver.org/) as enforced by `elm publish`.
Elm packages start at 1.0.0, so there are no 0.x releases; the first
published version will be 1.0.0.

## [Unreleased]

### Added

- `Xlsx.Write`: build workbooks with sheets, dense rows (`row`, `rowFrom`,
  `emptyRow`, `emptyRows`) and cells (`string`, `float`, `int`, `bool`,
  `date`, `dateTime`, `blank`, `formula`, `formulaWithResult`, `error`), then
  `toBytes`. Dates get a default `yyyy-mm-dd` format. Newer functions get
  their `_xlfn.` prefix. Every problem (invalid sheet names, NaN, dates out of
  range, Excel's size limits, ...) is reported at once with its location.
  `Auto` compression stores parts over 4 MB to keep large exports fast.
- `Xlsx.Style`: fonts (bold, italic, underline, strikethrough, size, color,
  name), solid fills, borders, alignment (horizontal, vertical, wrap, indent,
  rotation), number formats with helpers (`currency`, `percent`,
  `thousands`, ...) and colors (`rgb`, `hex`). Styles layer field by field.
- `Xlsx.Write`: `withStyle`, column widths, hidden columns, column styles,
  row heights, frozen panes, merged cells and autofilters.
- `Xlsx.Encode`: `table` turns a list of records into a sheet with a frozen,
  filterable header and fitted column widths.
- `Xlsx.Read`: open `.xlsx` files (`read`, `readWith` with size and
  zip-bomb limits), list sheets with their visibility and kind, and parse
  worksheets on demand (`sheet`, `firstSheet`). Read `rows`, `cells`, one
  `cell`, `dimensions` and `merges`. Cells carry their value, formula and
  number format; dates are recognised from number formats in both date
  systems. Handles files from Excel, openpyxl, XlsxWriter, the .NET Open XML
  SDK (prefixed XML) and ISO 29500 Strict. Legacy `.xls`, encrypted files,
  `.xlsb` and `.ods` are refused with a clear `Error`.
- `Xlsx.Decode`: decode sheet rows into your own types, in the style of
  `elm-csv` (`field`, `optionalField`, `column`, `into`/`pipeline`,
  `oneOf`, `andThen`, `blank`, `rowNumber`, ...). Headers can come from the
  first row, a custom list, or not be used. All failing rows are reported
  with sheet, row, column letter, header and the value found.
- `Xlsx.Date`: Excel serial numbers in the 1900 and 1904 date systems.
- `Xlsx.Ref`: `A1` references, ranges and sheet-name quoting.

### Fixed

- Writing text that ends in an emoji no longer hangs (`String.right` split
  the surrogate pair, and `String.any` never returns on a lone surrogate).
