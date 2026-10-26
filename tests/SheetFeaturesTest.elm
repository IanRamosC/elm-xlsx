module SheetFeaturesTest exposing (suite)

import Expect
import Helpers exposing (contains, containsAll)
import Test exposing (Test, describe, test)
import Xlsx.Style as Style
import Xlsx.Write as W


sheet1 : String
sheet1 =
    "xl/worksheets/sheet1.xml"


withRows : List W.Row -> (W.Sheet -> W.Sheet) -> W.Workbook
withRows rs f =
    W.workbook [ W.sheet "Data" rs |> f ]


problemsOf : W.Workbook -> List String
problemsOf wb =
    case W.toBytes W.defaultOptions wb of
        Ok _ ->
            []

        Err problems ->
            List.map W.problemToString problems


suite : Test
suite =
    describe "sheet features"
        [ columnTests
        , rowTests
        , panes
        , merges
        , autoFilters
        ]


columnTests : Test
columnTests =
    describe "columns"
        [ test "width is converted like Excel and marked custom" <|
            \_ ->
                withRows [] (W.columnWidth 1 20)
                    |> contains sheet1 "<cols><col min=\"1\" max=\"1\" width=\"20.7109375\" customWidth=\"1\"/></cols>"
        , test "neighboring columns with the same settings share one col" <|
            \_ ->
                withRows [] (W.columnWidths [ 12, 12, 12, 30 ])
                    |> contains sheet1 "<cols><col min=\"1\" max=\"3\" width=\"12.7109375\" customWidth=\"1\"/><col min=\"4\" max=\"4\" width=\"30.7109375\" customWidth=\"1\"/></cols>"
        , test "cols come before sheetData" <|
            \_ ->
                withRows [ W.row [ W.int 1 ] ] (W.columnWidth 1 20)
                    |> contains sheet1 "</cols><sheetData>"
        , test "hidden column keeps the default width" <|
            \_ ->
                withRows [] (W.hideColumn 2)
                    |> contains sheet1 "<col min=\"2\" max=\"2\" width=\"9.140625\" hidden=\"1\"/>"
        , test "column style is applied to the col and layered under cells" <|
            \_ ->
                withRows
                    [ W.row [ W.float 1, W.float 2 |> W.withStyle (Style.none |> Style.bold) ] ]
                    (W.columnStyle 1 (Style.none |> Style.numberFormat (Style.decimal 2))
                        >> W.columnStyle 2 (Style.none |> Style.numberFormat (Style.decimal 2))
                    )
                    |> Expect.all
                        [ contains sheet1 "<col min=\"1\" max=\"2\" width=\"9.140625\" style=\"1\"/>"
                        , contains sheet1 "<c r=\"A1\" s=\"1\"><v>1</v></c><c r=\"B1\" s=\"2\"><v>2</v></c>"
                        , contains "xl/styles.xml" "<xf numFmtId=\"2\" fontId=\"1\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyNumberFormat=\"1\" applyFont=\"1\"/>"
                        ]
        , test "invalid widths and columns" <|
            \_ ->
                withRows [] (W.columnWidth 0 10 >> W.columnWidth 2 300)
                    |> problemsOf
                    |> Expect.equal
                        [ "Sheet \"Data\": Column 0 is outside Excel's columns (1 to 16384)."
                        , "Sheet \"Data\": Column B has width 300; widths go from 0 to 255."
                        ]
        ]


rowTests : Test
rowTests =
    describe "row heights"
        [ test "a row with cells" <|
            \_ ->
                withRows [ W.row [ W.int 1 ] |> W.rowHeight 30 ] identity
                    |> contains sheet1 "<row r=\"1\" spans=\"1:1\" ht=\"30\" customHeight=\"1\">"
        , test "empty rows with a height are written" <|
            \_ ->
                withRows [ W.emptyRows 2 |> W.rowHeight 5, W.row [ W.int 1 ] ] identity
                    |> containsAll sheet1 [ "<row r=\"1\" ht=\"5\" customHeight=\"1\"/><row r=\"2\" ht=\"5\" customHeight=\"1\"/><row r=\"3\"", "<dimension ref=\"A1:A3\"/>" ]
        , test "spans cover blocks of 16 rows" <|
            \_ ->
                withRows [ W.row [ W.int 1 ], W.rowFrom 3 [ W.int 2 ], W.emptyRows 14, W.rowFrom 2 [ W.int 3 ] ] identity
                    |> contains sheet1 "<row r=\"1\" spans=\"1:3\"><c r=\"A1\"><v>1</v></c></row><row r=\"2\" spans=\"1:3\"><c r=\"C2\"><v>2</v></c></row><row r=\"17\" spans=\"2:2\">"
        , test "invalid height" <|
            \_ ->
                withRows [ W.row [ W.int 1 ] |> W.rowHeight 500 ] identity
                    |> problemsOf
                    |> Expect.equal [ "Sheet \"Data\": Row 1 has height 500; heights go from 0 to 409 points." ]
        ]


panes : Test
panes =
    describe "frozen panes"
        [ test "header row" <|
            \_ ->
                withRows [] (W.freeze { rows = 1, columns = 0 })
                    |> contains sheet1 "<sheetView tabSelected=\"1\" workbookViewId=\"0\"><pane ySplit=\"1\" topLeftCell=\"A2\" activePane=\"bottomLeft\" state=\"frozen\"/><selection pane=\"bottomLeft\"/></sheetView>"
        , test "first column" <|
            \_ ->
                withRows [] (W.freeze { rows = 0, columns = 1 })
                    |> contains sheet1 "<pane xSplit=\"1\" topLeftCell=\"B1\" activePane=\"topRight\" state=\"frozen\"/><selection pane=\"topRight\"/>"
        , test "both" <|
            \_ ->
                withRows [] (W.freeze { rows = 3, columns = 6 })
                    |> contains sheet1 "<pane xSplit=\"6\" ySplit=\"3\" topLeftCell=\"G4\" activePane=\"bottomRight\" state=\"frozen\"/><selection pane=\"topRight\" activeCell=\"G1\" sqref=\"G1\"/><selection pane=\"bottomLeft\" activeCell=\"A4\" sqref=\"A4\"/><selection pane=\"bottomRight\"/>"
        , test "nothing frozen" <|
            \_ ->
                withRows [] (W.freeze { rows = 0, columns = 0 })
                    |> contains sheet1 "<sheetView tabSelected=\"1\" workbookViewId=\"0\"/>"
        , test "negative" <|
            \_ ->
                withRows [] (W.freeze { rows = -1, columns = 0 })
                    |> problemsOf
                    |> Expect.equal [ "Sheet \"Data\": Frozen rows and columns cannot be negative." ]
        ]


merges : Test
merges =
    describe "merged cells"
        [ test "mergeCells after sheetData; hidden cells lose their value but keep style" <|
            \_ ->
                withRows [ W.row [ W.string "Title", W.int 2 |> W.withStyle (Style.none |> Style.bold), W.int 3 ] ] (W.merge "A1:C1")
                    |> containsAll sheet1
                        [ "<c r=\"A1\" t=\"s\"><v>0</v></c><c r=\"B1\" s=\"1\"/></row>"
                        , "</sheetData><mergeCells count=\"1\"><mergeCell ref=\"A1:C1\"/></mergeCells><pageMargins"
                        ]
        , test "invalid, single-cell and overlapping merges" <|
            \_ ->
                withRows [] (W.merge "A1:B2" >> W.merge "nope" >> W.merge "C3" >> W.merge "B2:D4")
                    |> problemsOf
                    |> Expect.equal
                        [ "Sheet \"Data\": The merge range \"nope\" is not a valid range like A1:C1."
                        , "Sheet \"Data\": The merge range C3 covers a single cell; merges need at least two."
                        , "Sheet \"Data\": The merge range B2:D4 overlaps A1:B2."
                        ]
        ]


autoFilters : Test
autoFilters =
    describe "autofilter"
        [ test "element and hidden defined name" <|
            \_ ->
                W.workbook [ W.sheet "One" [], W.sheet "Q1 Sales" [] |> W.autoFilter "A1:D51" ]
                    |> Expect.all
                        [ contains "xl/worksheets/sheet2.xml" "<sheetData/><autoFilter ref=\"A1:D51\"/><pageMargins"
                        , contains "xl/workbook.xml" "</sheets><definedNames><definedName name=\"_xlnm._FilterDatabase\" localSheetId=\"1\" hidden=\"1\">'Q1 Sales'!$A$1:$D$51</definedName></definedNames><calcPr"
                        ]
        , test "invalid range" <|
            \_ ->
                withRows [] (W.autoFilter "A1:")
                    |> problemsOf
                    |> Expect.equal [ "Sheet \"Data\": The autofilter range \"A1:\" is not a valid range like A1:D20." ]
        ]
