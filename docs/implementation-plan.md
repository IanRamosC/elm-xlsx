# Plan: `elm-xlsx`: pure-Elm XLSX export and import

## Context

The Elm registry (2,084 packages, checked 2026-10-14) has **no XLSX package**. Business apps that need "Export to Excel" or "Import spreadsheet" either send the job to a server or embed SheetJS through ports. XLSX is a ZIP of XML files. Elm already has the hard low-level pieces (`agu-z/elm-zip` reads and writes ZIP, `folkertdev/elm-flate` does deflate/inflate, `elm/bytes` handles bytes), so a pure-Elm implementation is feasible: bytes in, bytes out, no ports. The goal is a publishable package with an API in the style of `Json.Encode`/`Json.Decode` whose files open in Excel without the "We found a problem with some content" repair prompt.

**Assumptions:** the package lives in `/Users/ianramos/projects/ianramosc/elm/` (currently empty), named `ianramosc/elm-xlsx`, license BSD-3-Clause, Elm 0.19.1.

## Scope decision for v1.0

The user asked whether "Practical basics" is enough or v1 should be "Rich". **Recommendation: Practical basics + hyperlinks + document properties.** Defer data validation and conditional formatting to 1.1 and 1.2.

- Hyperlinks are common in exports (links back to records) and cheap: one relationships file plus one worksheet element.
- Document properties (title, author, created date) are trivial and expected by enterprise users.
- Data validation and conditional formatting are **additive**. They occupy fixed slots in the worksheet's schema-ordered elements, and conditional formatting uses the `dxfs` table in `styles.xml`. If v1 already writes those parts in schema order and leaves the `dxfs` slot (empty), both can ship later as minor versions with no breaking API change. Holding 1.0 back for them doubles the scope without changing the architecture.

**In v1.0**
- **Write:** strings, numbers, booleans, dates and date-times, formulas (with an optional cached value), error values, blanks; number and date formats; font (bold, italic, underline, size, color, name); fill color; borders; alignment and wrap; column widths and hidden columns; row heights; frozen panes; merged cells; autofilter; multiple sheets and the active sheet; hyperlinks (external and internal); core/app document properties.
- **Read:** every sheet's cell values with types, date detection, formulas (cached value plus formula text), merged ranges, sheet visibility, the 1900 and 1904 date systems; a raw grid API plus a typed `Xlsx.Decode` layer.

**Deferred (roadmap):** data validation (1.1), conditional formatting (1.2), Excel tables, comments, images and charts, rich-text runs within one cell, dynamic-array (spill) formulas, protection and encryption, reading styles back as `Style` values, streaming very large files.

## Reference libraries and what we adopt from each

| Library | Ecosystem | Role | Practices we adopt |
|---|---|---|---|
| **XlsxWriter / rust_xlsxwriter** (J. McNamara) | Python / Rust | Write-only; best-known for Excel-identical output | Package part layout, **schema element ordering**, style deduplication, shared strings table, column-width conversion, `_xHHHH_` string escaping including `_x005F_`, `_xlfn.` prefixes for newer functions, Excel limits enforced at write time, **comparing generated XML against Excel-made files**, plus its BSD-2 corpus of 1,000 Excel-generated reference files (`xlsxwriter/test/comparison/xlsx_files`) |
| **openpyxl** | Python | Read/write | `is_date_format` heuristic, date serial ↔ datetime conversion (1900 leap-year bug, 1904 system, rounding to milliseconds), read-only/lazy sheet loading, refusing DTDs (defusedxml approach) |
| **calamine** | Rust | Fast read-only | Find parts **through relationships, with default-path fallbacks**, match XML by local name (ignore prefixes), **don't trust `<dimension>`**, parse sheets lazily one at a time; `python-calamine` as the **test oracle** |
| **SheetJS** | JS | Widest compatibility | Detect file type by signature with clear errors for `.xls`, `.xlsb` and encrypted files; tolerate odd writers (inline strings, missing `r` attributes); cell types n/s/b/e/d plus blank |
| **ExcelJS** | JS | Styled exports | Column-definition API (`header`, `key`, `width`) → inspires `Xlsx.Encode.table` |
| **Apache POI** | Java | OPC/OOXML reference | Part names are **case-insensitive** and resolved relative to their source part; **zip-bomb guard** (`ZipSecureFile`: minimum inflate ratio 0.01, maximum entry size) |
| **BrianHicks/elm-csv** | Elm | Closest Elm precedent | `Csv.Decode` API shape (`column`, `field`, `optionalField`, `pipeline`, `into`, `oneOf`), `FieldNames` options, errors that carry row and column location |

**Elm dependencies (all verified on the registry):** `agu-z/elm-zip` 3.0.1 (2022), `folkertdev/elm-flate` 2.0.6 (Apr 2026), `miniBill/elm-xml-parser` 1.0.2 (Dec 2024; the maintained fork of jinjor's), `elm/parser`, `elm/bytes`, `elm/time`, `justinmimbs/date` 4.1.0. Test-only: `elm-explorations/test` 2.2.1, `elm-explorations/benchmark`, `danfishgold/base64-bytes`.
`ymtszw/elm-xml-decode` was checked and **rejected**: its path matching uses exact tag names, so files that use `x:`-prefixed tags (written by .NET's Open XML SDK) would fail.

**elm-zip facts confirmed in its source:** the reader takes entry sizes from the central directory, so files written with data descriptors read correctly. The writer puts sizes in the local headers, writes no data descriptors and keeps entry order, which is what Excel wants. **No ZIP64**, which is acceptable because XLSX files over 4 GB are out of scope. Each entry's `Meta.lastModified` needs a `(Zone, Posix)` value. If a blocking bug turns up, copy the module into `Internal/` (BSD-3 allows this with attribution). Elm can't depend on git forks.

## Package layout

```
elm/
  elm.json  README.md  LICENSE  CHANGELOG.md
  src/
    Xlsx/Write.elm        -- Workbook/Sheet/Row/Cell builders, toBytes
    Xlsx/Style.elm        -- Style record and combinators, number-format helpers
    Xlsx/Encode.elm       -- record list → Sheet (column definitions, ExcelJS-like)
    Xlsx/Read.elm         -- Bytes → Workbook (lazy sheets), raw Cell grid
    Xlsx/Decode.elm       -- elm-csv-style typed row decoders
    Xlsx/Ref.elm          -- "B3" ↔ {column, row}, ranges, column letters
    Xlsx/Date.elm         -- Excel serial ↔ Date / date-time (1900 and 1904)
    Xlsx/Internal/
      Xml/Escape.elm      -- text/attribute escaping, _xHHHH_, xml:space rules
      Xml/Builder.elm     -- string-list XML builder (no DOM)
      Xml/Tree.elm        -- XmlParser wrapper: local-name matching, DTD rejection
      Opc.elm             -- [Content_Types], .rels writing, part-path resolution
      Limits.elm          -- Excel limits and validation `Problem`s
      SharedStrings.elm   -- write (dedupe) and read (rich text runs, rPh skip)
      Styles.elm          -- write (dedupe tables) and read (numFmt per xf)
      NumFmt.elm          -- built-in format table, date-format detection
      FutureFunctions.elm -- _xlfn. prefix list
      SheetWriter.elm     -- worksheet XML in schema order
      SheetScanner.elm    -- fast sheetData reader (elm/parser, no DOM)
  tests/   benchmarks/   examples/ (upload-and-download app using elm/file)
  scripts/ (fixture embedding, oracle comparison, validators; Python/Node/.NET, never shipped)
```

The package depends only on `Bytes`, not on `elm/file`. File picking and downloading stay in the app (shown in `examples/`), which keeps the package free of effects and fully testable.

## Public API sketch (signatures only, to be finalized during implementation)

```elm
-- Xlsx.Write
workbook : List Sheet -> Workbook
sheet : String -> List Row -> Sheet
row : List Cell -> Row                    -- plus rowWithHeight
string, float, int, bool, blank, error, date, dateTime, formula, hyperlink : ... -> Cell
withStyle : Style -> Cell -> Cell
withColumns : List Column -> Sheet -> Sheet   -- width, hidden, default style
freeze : { rows : Int, columns : Int } -> Sheet -> Sheet
merge, autoFilter : Range -> Sheet -> Sheet
withProperties : Properties -> Workbook -> Workbook
toBytes : Options -> Workbook -> Result (List Problem) Bytes
-- Options: { created : Posix, compression : Deflate | Store }. The caller passes the
-- time explicitly (no Time.now inside the package), so output is deterministic.

-- Xlsx.Encode
table : List (ColumnDef a) -> List a -> String -> Sheet   -- header row, widths, frozen header, autofilter

-- Xlsx.Read
read : ReadOptions -> Bytes -> Result Error Workbook      -- parses metadata, styles, shared strings
sheetNames : Workbook -> List { name : String, visibility : Visibility }
sheet : String -> Workbook -> Result Error Sheet          -- parses one worksheet on demand
rows : Sheet -> List (List Cell)  -- also: cellAt : Ref -> Sheet -> Maybe Cell, merges, dimensions
type CellValue = Text String | Number Float | Boolean Bool | DateValue Date | DateTime Date Int | ErrorValue String | Empty
-- Cell = { value : CellValue, formula : Maybe String, numberFormat : Maybe String }

-- Xlsx.Decode (mirrors Csv.Decode)
string, int, float, bool, date, dateTime, cell, blank : Decoder ...
column : Int -> Decoder a -> Decoder a ; field, optionalField : String -> ...
map, map2, map3, into, pipeline, oneOf, andThen, fail, succeed
decodeSheet : FieldNames -> Decoder a -> Xlsx.Read.Sheet -> Result Error (List a)
-- Errors carry the sheet name, 1-based row, column letter and expected/actual value.
```

Coordinates are **1-based** throughout to match Excel (`A1` = column 1, row 1). `Xlsx.Ref` converts to and from 0-based indexes.

## Writer: implementation rules

**Package parts.** Zip entries in this order, `[Content_Types].xml` first: `[Content_Types].xml`, `_rels/.rels`, `docProps/app.xml`, `docProps/core.xml`, `xl/workbook.xml`, `xl/_rels/workbook.xml.rels`, `xl/styles.xml`, `xl/sharedStrings.xml` (only if strings are used), `xl/worksheets/sheetN.xml`, `xl/worksheets/_rels/sheetN.xml.rels` (only if the sheet has external hyperlinks). No theme part: the default font is set explicitly as Calibri 11, the same as XlsxWriter's default and the basis of the width math. Every part gets a content-type override and every link gets a relationship. Zip timestamps use `Options.created`, so the same input always produces the same bytes.

**Worksheet element order** (`CT_Worksheet` sequence; elements out of order trigger Excel's repair prompt). Emit in this fixed order and skip empty ones:
`sheetPr → dimension → sheetViews → sheetFormatPr → cols → sheetData → sheetProtection* → autoFilter → mergeCells → conditionalFormatting* → dataValidations* → hyperlinks → printOptions* → pageMargins → pageSetup* → headerFooter* → drawing* → tableParts* → extLst*` (* = slot reserved for later versions). `pageMargins` is always written with Excel's default values, as XlsxWriter does.

**styles.xml** in schema order: `numFmts, fonts, fills, borders, cellStyleXfs, cellXfs, cellStyles, dxfs, tableStyles`, each with a `count` attribute. Mandatory defaults: font 0 = default font; **fills 0 = `none` and 1 = `gray125` are always reserved**; border 0 = empty; `cellStyleXfs[0]`; `cellXfs[0]` = default; `cellStyles` contains `Normal` (builtinId 0), plus `Hyperlink` (builtinId 8) when hyperlinks are used; `dxfs count="0"`. Deduplicate fonts, fills, borders, numFmts and xfs separately by canonical key. Custom numFmt ids start at **164**; a format string that exactly matches a built-in one (ids 0–49) reuses that id. Set `applyFont`/`applyFill`/`applyBorder`/`applyAlignment`/`applyNumberFormat="1"` where they apply. Colors are written as `FFRRGGBB`.

**Cells.**
- Rows in ascending `r` order, cells in ascending column order, every cell with an explicit `r="B3"`. `<dimension>` is correct (`A1` for an empty sheet).
- **Strings** go in the shared strings table (deduplicated, with `count` and `uniqueCount`), as Excel and XlsxWriter do. Escaping: `& < >` in text, plus `"` in attributes. XML-illegal control characters (0x00–0x08, 0x0B, 0x0C, 0x0E–0x1F) become `_xHHHH_`, and a literal `_xHHHH_` already in user text gets its underscore escaped as `_x005F_`. Leading or trailing whitespace, or newlines, add `xml:space="preserve"`. Newlines only display with a wrap style; the docs must say so.
- **Numbers** are written with `String.fromFloat` (shortest form that reads back exactly; `1e+21` is valid `xsd:double`). NaN and ±Infinity are rejected as a `Problem`. `-0` is written as `0`.
- **Booleans:** `t="b"` with `1`/`0`. **Errors:** `t="e"` with only the 7 standard codes (`#NULL! #DIV/0! #VALUE! #REF! #NAME? #NUM! #N/A`).
- **Dates and date-times** become serial numbers in the 1900 system. **A date cell with no format gets a default format automatically** (`yyyy-mm-dd`, or `yyyy-mm-dd hh:mm:ss`): a bare serial number is the most common pitfall. Dates before 1900-01-01 are rejected as a `Problem`.
- **Formulas:** strip a leading `=`; write `<f>` plus a `<v>` cached value when given (`t="str"`, `t="b"` or `t="e"` according to its type). Without a cached value, set `<calcPr fullCalcOnLoad="1"/>` so Excel and LibreOffice recalculate when opening. Add `_xlfn.` prefixes automatically for newer functions (XLOOKUP, XMATCH, IFS, SWITCH, MAXIFS, MINIFS, CONCAT, TEXTJOIN, LET, STDEV.S, …). Formulas are not parsed; the docs require English function names, comma separators and A1 references. Dynamic-array spilling (FILTER, UNIQUE, SORT, which need `metadata.xml`) is out of scope for v1, and the docs must say so.
- **Column widths:** the user gives Excel character units. Convert them the way XlsxWriter does (max digit width 7 px for Calibri 11, 5 px padding), set `customWidth="1"`, and merge neighboring columns with identical settings into one `<col min max>`. **Row heights:** `ht` plus `customHeight="1"`.
- **Frozen panes:** `<pane xSplit ySplit topLeftCell activePane state="frozen"/>` plus a matching `<selection pane>`. The active sheet gets `tabSelected="1"` and `bookViews/workbookView activeTab`.
- **Merges:** at least 2 cells and no overlaps (validated). Only the top-left cell keeps its value.
- **Autofilter:** `<autoFilter ref>` plus the defined name `_xlnm._FilterDatabase` (`localSheetId`, `hidden="1"`).
- **Hyperlinks:** external links use a sheet relationship with `TargetMode="External"`. Internal links (`'Sheet 2'!A1`) use the `location` attribute and no relationship. Hyperlink cells get the built-in Hyperlink style (blue, underlined). Validated limits: URL ≤ 2,079 characters, ≤ 65,530 links per sheet.
- **Sheet names in references** are quoted (`'My Sheet'!A1`) with internal `'` doubled.

**Validation (`Xlsx.Internal.Limits`), collected and returned as a `List Problem` rather than stopping at the first:** 1,048,576 rows, 16,384 columns, 32,767 characters per cell (Elm's `String.length` counts UTF-16 units, which is what Excel counts), sheet names of 1–31 characters without `: \ / ? * [ ]`, not starting or ending with `'`, unique ignoring case, not `History`; at least one sheet; finite numbers; dates in range; no overlapping merges; row height ≤ 409 pt; column width ≤ 255.

## Reader: implementation rules

1. **Detect the file type from its first bytes:** `PK\x03\x04` → continue. `D0 CF 11 E0 A1 B1 1A E1` → `EncryptedOrLegacyXls` (a password-protected `.xlsx` and an old `.xls` share this container). Anything else → `NotAnXlsxFile`.
2. **Zip safety (POI's rules):** before inflating, check the central directory against `ReadOptions.maxUncompressedBytes` (default 256 MB) and reject entries whose compressed/uncompressed ratio is below 0.01. Inflate only the parts needed. Reject any XML part containing `<!DOCTYPE` (OOXML never uses one, and this blocks entity-expansion attacks whatever the XML parser does).
3. **XML decoding:** UTF-8 via `Bytes.Decode.string`, with any BOM stripped. A UTF-16 BOM → `UnsupportedEncoding` (very rare). Small parts are parsed with `miniBill/elm-xml-parser` through `Internal.Xml.Tree`, which matches **local names** so `x:row` equals `row`.
4. **Find parts through relationships, not hard-coded paths:** `_rels/.rels` → the officeDocument target. Accept both the Transitional (`schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument`) and Strict (`purl.oclc.org/ooxml/officeDocument/relationships/officeDocument`) relationship types. Resolve targets relative to the source part's folder (handle `../`, absolute `/xl/...` and percent-encoding) and look parts up ignoring case. Fall back to the default paths (`xl/workbook.xml`, `xl/styles.xml`, `xl/sharedStrings.xml`) when a relationship is missing, as calamine does. A main content type of `.xlsb` → `UnsupportedFormat "xlsb"`. `.xlsm` is read normally and macros are ignored.
5. **workbook.xml:** sheet name, `sheetId`, `r:id` (attribute matched by local name `id`) and `state` (visible/hidden/veryHidden); `workbookPr/@date1904`. Chartsheets and dialogsheets (detected by relationship type) are listed with their kind and not parsed.
6. **sharedStrings.xml:** each `<si>` is either `<t>` or the concatenated `<r><t>` runs. Skip `<rPh>` (phonetic hints). Decode `_xHHHH_` escapes. Keep whitespace exactly.
7. **styles.xml:** for each `cellXfs` index, record its numFmt id and format string and compute `isDate` once. **Date detection** as in openpyxl/SheetJS: built-in ids 14–22 and 45–47, plus locale-specific 27–36 and 50–58. For custom codes, take the first `;`-separated section and remove quoted text, `\x` escapes and `[...]` blocks (except elapsed-time `[h] [m] [s]`), then look for any of `y m d h s` (ignoring case). `General` is never a date.
8. **Worksheet (on demand):** `Internal.SheetScanner` reads `<sheetData>` with elm/parser and **builds no XML tree** (calamine/openpyxl read-only approach), skipping unknown elements.
   - Per cell, read `r` (if missing: previous column + 1; for a row, previous row + 1), `t` (`n` default, `s`, `str`, `inlineStr`, `b`, `e`, `d`), `s`, `<v>`, `<f>` and `<is>`.
   - Shared formulas: keep the cached value; the formula text is `Nothing` for dependent cells in v1.
   - Also read `<mergeCells>`. Ignore `<dimension>`; compute the extent from the cells.
   - `t="d"`: parse ISO 8601. Serial dates: convert with `Xlsx.Date`, using the workbook's date system and rounding to the millisecond.
   - Skip trailing empty rows and cells when building the grid.
9. **Errors:** `NotAnXlsxFile | EncryptedOrLegacyXls | UnsupportedFormat String | UnsupportedEncoding String | MissingPart String | InvalidXml { part, problem } | LimitExceeded String | SheetNotFound String`, each with `errorToString`.

**Date serials (`Xlsx.Date`):**
- 1900 system: serials 1–59 = 1899-12-31 + n. Serial 60 = Excel's non-existent 1900-02-29, read as 1900-03-01 (documented) and never written. Serials ≥ 61 = 1899-12-30 + n. Serial 0 with a fraction = time only.
- 1904 system: 1904-01-01 + n.
- The fraction × 86,400,000 is rounded to the nearest millisecond.
- Posix values convert only through an explicit `Time.Zone`, because Excel stores local wall-clock time.

## Performance and robustness

- **No non-tail recursion over rows or cells.** Use `List.foldl`, `Array` and `Parser.loop`. A fuzz test with 200k cells checks stack safety.
- **XML is built as a reversed `List String`, then `String.concat`, then one `Bytes.Encode.string`.** The benchmark compares this against a sequence of `Bytes.Encode` pieces.
- **The writer offers `compression = Store`** for very large exports when deflate in pure Elm is too slow. It produces a valid, just bigger, file.
- **M0 must pass these targets on a mid-range laptop in Chrome before the rest of the work starts:**
  - inflating a 10 MB sheet XML in under 2 s
  - deflating it in under 4 s
  - scanning 100k cells in under 2 s
  - writing 100k cells in under 3 s

  If deflate misses its target, the default becomes `Store` above a size threshold. If the scanner misses, write a hand-rolled scanner using `String.indexes`/`String.slice`.

## Testing and verification

1. **Unit tests (elm-test):**
   - Escaping, including control characters, `_x` sequences and whitespace.
   - `Xlsx.Ref`: A, Z, AA, AZ, XFD = 16384, invalid input.
   - Date serials: 1900-01-01 = 1, 1900-02-28 = 59, 60, 1900-03-01 = 61, 2000-01-01 = 36526, 2026-11-13 = 46339, 1904 system 0 = 1904-01-01, time fractions.
   - Date-format detection, using openpyxl's test list of format codes.
   - Sheet-name validation, style deduplication, column-range merging, `_xlfn.` prefixing.
2. **Fuzz round trips:** a random workbook goes through `toBytes`, then `read`, then `rows` and must come back equal. Covers Unicode, emoji, control characters, whitespace, large and small floats, dates, booleans, formulas with cached values, empty sheets, and 1-cell and 200k-cell sheets.
3. **Golden XML tests (XlsxWriter's method):** for each feature, build the matching workbook and compare each XML part, normalized (attribute order and whitespace ignored, `docProps` timestamps ignored), against the Excel-made reference file from XlsxWriter's BSD-2 corpus. Candidate references: `simple01`, `format01…`, `merge_range01`, `autofilter01`, `panes01`, `set_column01`, `hyperlink01`, `date_1904_01`, `formula_results01`. The corpus is credited in `tests/fixtures/LICENSE`.
4. **Read fixtures:** files from Excel (XlsxWriter corpus), LibreOffice, Google Sheets, Apple Numbers, openpyxl, SheetJS, ExcelJS, excelize, Apache POI and Open XML SDK (`x:`-prefixed tags). Edge cases: inline strings, missing `r`, Strict OOXML, 1904 dates, rich-text shared strings, hidden sheets, chartsheets, an encrypted file, `.xls`, `.xlsb`, a zip bomb, a file with a DTD. elm-test has no file access, so `scripts/embed-fixtures` turns them into `tests/Fixtures/*.elm` base64 modules. **Differential oracle:** `scripts/oracle.py` dumps every fixture with `python-calamine` (and openpyxl) to JSON, and the Elm results must match.
5. **External validation of generated files (CI scripts):**
   - openpyxl loads each file and values match.
   - `python-calamine` reads it.
   - LibreOffice headless `soffice --headless --convert-to csv` succeeds and the CSV matches.
   - The **Open XML SDK validator** (a small .NET tool around `OpenXmlValidator`) reports **0 errors**.
   - Node runs the Elm side through a `Platform.worker` harness.
6. **Manual release gate:** the example workbooks open with **no repair prompt** in Excel for Windows, Excel for Mac and Excel on the web, import correctly into Google Sheets, and open in Apple Numbers.
7. **Benchmarks** (`benchmarks/`, elm-explorations/benchmark) for the M0 targets, re-run before each release.
8. **Publish checks:** elm-format, elm-review (with the elm-review-unused, elm-review-simplify, elm-review-documentation and elm-review-common rule packages), `elm make --docs=docs.json` with every exposed value documented, a README with complete examples, and `elm publish` dry run.

## Milestones

| # | Deliverable | Exit criterion |
|---|---|---|
| M0 | Performance and compatibility spike: elm-flate on real sheet sizes, scanner prototype, string building; a minimal hand-made 1-cell file opened in Excel | Targets met or fallbacks chosen |
| M1 | Writer, values only: parts, shared strings, cells, dates with default format, formulas, validation | Golden `simple01` matches; validator reports 0 errors; Excel opens it with no repair prompt |
| M2 | Styles plus sheet features: fonts, fills, borders, alignment, numFmts, columns, rows, panes, merges, autofilter, multiple sheets; `Xlsx.Encode.table` | Golden tests for each feature pass |
| M3 | Reader raw grid: file-type detection, zip safety, OPC resolution, shared strings, styles/date detection, scanner, merges, lazy sheets | All read fixtures match the calamine oracle |
| M4 | `Xlsx.Decode` | Decoder tests include error locations |
| M5 | Hyperlinks, document properties, fuzz round trips, security fixtures | Fuzz passes at 200k cells; all security fixtures rejected cleanly |
| M6 | Documentation, `examples/` app (elm/file upload → decode → table → export → download), CHANGELOG | Manual Excel/Sheets/Numbers gate passes; publish 1.0.0 |

## Risks

- **Pure-Elm deflate/inflate speed** on large files. Mitigated by testing it first in M0, the `Store` option, and lazy per-sheet inflation.
- **elm-zip maintenance** (last release 2022). Mitigated by tests against fixtures from many writers; copy it into `Internal/` if needed.
- **Excel is the only real judge of "no repair prompt",** and it can't run in CI. Mitigated by the Open XML SDK validator plus golden XML tests against Excel's own output, with a manual gate before each release.
