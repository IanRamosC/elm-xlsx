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
- `Xlsx.Date`: Excel serial numbers in the 1900 and 1904 date systems.
- `Xlsx.Ref`: `A1` references, ranges and sheet-name quoting.
