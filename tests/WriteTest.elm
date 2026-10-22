module WriteTest exposing (suite)

import Base64
import Bytes
import Date
import Expect
import Helpers exposing (contains, containsAll, part, partNames, writeOk)
import Test exposing (Test, describe, test)
import Time exposing (Month(..))
import Xlsx.Date as XDate
import Xlsx.Write as W


sheet1 : String
sheet1 =
    "xl/worksheets/sheet1.xml"


one : List W.Cell -> W.Workbook
one cs =
    W.workbook [ W.sheet "Sheet1" [ W.row cs ] ]


problemsOf : W.Workbook -> List String
problemsOf wb =
    case W.toBytes W.defaultOptions wb of
        Ok _ ->
            []

        Err problems ->
            List.map W.problemToString problems


suite : Test
suite =
    describe "Xlsx.Write"
        [ package
        , cellTests
        , layout
        , formulas
        , validation
        , options
        ]


package : Test
package =
    describe "package"
        [ test "parts in order, sharedStrings only when needed" <|
            \_ ->
                writeOk (one [ W.int 1 ])
                    |> Result.map partNames
                    |> Expect.equal
                        (Ok
                            [ "[Content_Types].xml"
                            , "_rels/.rels"
                            , "docProps/app.xml"
                            , "docProps/core.xml"
                            , "xl/workbook.xml"
                            , "xl/_rels/workbook.xml.rels"
                            , "xl/styles.xml"
                            , "xl/worksheets/sheet1.xml"
                            ]
                        )
        , test "sharedStrings part and its content type and relationship" <|
            \_ ->
                one [ W.string "hi" ]
                    |> Expect.all
                        [ contains "[Content_Types].xml" "<Override PartName=\"/xl/sharedStrings.xml\""
                        , contains "xl/_rels/workbook.xml.rels" "Target=\"sharedStrings.xml\""
                        , contains "xl/sharedStrings.xml" "<sst xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" count=\"1\" uniqueCount=\"1\"><si><t>hi</t></si></sst>"
                        ]
        , test "sheets in workbook.xml" <|
            \_ ->
                W.workbook [ W.sheet "One" [], W.sheet "Two & Three" [] ]
                    |> containsAll "xl/workbook.xml"
                        [ "<sheet name=\"One\" sheetId=\"1\" r:id=\"rId1\"/>"
                        , "<sheet name=\"Two &amp; Three\" sheetId=\"2\" r:id=\"rId2\"/>"
                        ]
        , test "app.xml lists the sheet names" <|
            \_ ->
                W.workbook [ W.sheet "One" [], W.sheet "Two" [] ]
                    |> contains "docProps/app.xml" "<vt:i4>2</vt:i4></vt:variant></vt:vector></HeadingPairs><TitlesOfParts><vt:vector size=\"2\" baseType=\"lpstr\"><vt:lpstr>One</vt:lpstr><vt:lpstr>Two</vt:lpstr>"
        , test "first sheet selected by default" <|
            \_ ->
                W.workbook [ W.sheet "One" [], W.sheet "Two" [] ]
                    |> Expect.all
                        [ contains sheet1 "<sheetView tabSelected=\"1\" workbookViewId=\"0\"/>"
                        , contains "xl/worksheets/sheet2.xml" "<sheetView workbookViewId=\"0\"/>"
                        ]
        , test "active sheet" <|
            \_ ->
                W.workbook [ W.sheet "One" [], W.sheet "Two" [] ]
                    |> W.withActiveSheet "Two"
                    |> Expect.all
                        [ contains "xl/workbook.xml" "activeTab=\"1\""
                        , contains "xl/worksheets/sheet2.xml" "tabSelected=\"1\""
                        ]
        , test "empty sheet" <|
            \_ ->
                W.workbook [ W.sheet "Empty" [] ]
                    |> containsAll sheet1 [ "<dimension ref=\"A1\"/>", "<sheetData/>" ]
        ]


cellTests : Test
cellTests =
    describe "cells"
        [ test "strings are shared and deduplicated" <|
            \_ ->
                W.workbook [ W.sheet "S" [ W.row [ W.string "a", W.string "b", W.string "a" ] ] ]
                    |> Expect.all
                        [ contains "xl/sharedStrings.xml" "count=\"3\" uniqueCount=\"2\"><si><t>a</t></si><si><t>b</t></si></sst>"
                        , contains sheet1 "<c r=\"A1\" t=\"s\"><v>0</v></c><c r=\"B1\" t=\"s\"><v>1</v></c><c r=\"C1\" t=\"s\"><v>0</v></c>"
                        ]
        , test "strings are escaped" <|
            \_ ->
                one [ W.string "<b>&\u{0001} " ]
                    |> contains "xl/sharedStrings.xml" "<si><t xml:space=\"preserve\">&lt;b&gt;&amp;_x0001_ </t></si>"
        , test "numbers" <|
            \_ ->
                one [ W.float 1.5, W.int -3, W.float 1.0e21, W.float 0, W.float 0.1 ]
                    |> contains sheet1 "<c r=\"A1\"><v>1.5</v></c><c r=\"B1\"><v>-3</v></c><c r=\"C1\"><v>1e+21</v></c><c r=\"D1\"><v>0</v></c><c r=\"E1\"><v>0.1</v></c>"
        , test "booleans" <|
            \_ ->
                one [ W.bool True, W.bool False ]
                    |> contains sheet1 "<c r=\"A1\" t=\"b\"><v>1</v></c><c r=\"B1\" t=\"b\"><v>0</v></c>"
        , test "errors" <|
            \_ ->
                one [ W.error "#N/A" ]
                    |> contains sheet1 "<c r=\"A1\" t=\"e\"><v>#N/A</v></c>"
        , test "dates get a default format" <|
            \_ ->
                one [ W.date (Date.fromCalendarDate 2026 Nov 13) ]
                    |> Expect.all
                        [ contains sheet1 "<c r=\"A1\" s=\"1\"><v>46339</v></c>"
                        , containsAll "xl/styles.xml"
                            [ "<numFmts count=\"1\"><numFmt numFmtId=\"164\" formatCode=\"yyyy-mm-dd\"/></numFmts>"
                            , "<cellXfs count=\"2\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\"/><xf numFmtId=\"164\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyNumberFormat=\"1\"/></cellXfs>"
                            ]
                        ]
        , test "date-times" <|
            \_ ->
                one [ W.dateTime (XDate.dateTime (Date.fromCalendarDate 2000 Jan 1) 12 0 0), W.date (Date.fromCalendarDate 2000 Jan 1) ]
                    |> Expect.all
                        [ contains sheet1 "<c r=\"A1\" s=\"1\"><v>36526.5</v></c><c r=\"B1\" s=\"2\"><v>36526</v></c>"
                        , contains "xl/styles.xml" "<numFmt numFmtId=\"164\" formatCode=\"yyyy-mm-dd hh:mm:ss\"/><numFmt numFmtId=\"165\" formatCode=\"yyyy-mm-dd\"/>"
                        ]
        , test "unstyled blanks are not written" <|
            \_ ->
                one [ W.blank, W.int 1 ]
                    |> containsAll sheet1 [ "<row r=\"1\" spans=\"2:2\"><c r=\"B1\"><v>1</v></c></row>", "<dimension ref=\"B1\"/>" ]
        ]


layout : Test
layout =
    describe "layout"
        [ test "rows are numbered from 1 and empty rows skipped" <|
            \_ ->
                W.workbook
                    [ W.sheet "S"
                        [ W.row [ W.int 1 ]
                        , W.emptyRows 2
                        , W.row [ W.int 2 ]
                        , W.emptyRow
                        , W.row [ W.int 3 ]
                        ]
                    ]
                    |> containsAll sheet1
                        [ "<row r=\"1\" spans=\"1:1\"><c r=\"A1\"><v>1</v></c></row><row r=\"4\" spans=\"1:1\"><c r=\"A4\"><v>2</v></c></row><row r=\"6\" spans=\"1:1\"><c r=\"A6\"><v>3</v></c></row>"
                        , "<dimension ref=\"A1:A6\"/>"
                        ]
        , test "rowFrom starts at a column" <|
            \_ ->
                W.workbook [ W.sheet "S" [ W.rowFrom 3 [ W.string "x", W.int 5 ] ] ]
                    |> containsAll sheet1 [ "<c r=\"C1\" t=\"s\"><v>0</v></c><c r=\"D1\"><v>5</v></c>", "<dimension ref=\"C1:D1\"/>" ]
        , test "a row of blanks is skipped" <|
            \_ ->
                W.workbook [ W.sheet "S" [ W.row [ W.blank ], W.row [ W.int 1 ] ] ]
                    |> contains sheet1 "<sheetData><row r=\"2\" spans=\"1:1\">"
        , test "worksheet elements in schema order" <|
            \_ ->
                case writeOk (one [ W.int 1 ]) |> Result.toMaybe |> Maybe.andThen (part sheet1) of
                    Just xml ->
                        [ "<dimension", "<sheetViews", "<sheetFormatPr", "<sheetData", "<pageMargins" ]
                            |> List.map (\tag -> String.indexes tag xml |> List.head |> Maybe.withDefault -1)
                            |> Expect.all
                                [ \positions -> Expect.equal (List.sort positions) positions
                                , List.all (\p -> p >= 0) >> Expect.equal True
                                ]

                    Nothing ->
                        Expect.fail "no sheet"
        ]


formulas : Test
formulas =
    describe "formulas"
        [ test "leading = is dropped and recalculation requested" <|
            \_ ->
                one [ W.formula "=SUM(1,2)" ]
                    |> Expect.all
                        [ contains sheet1 "<c r=\"A1\"><f>SUM(1,2)</f></c>"
                        , contains "xl/workbook.xml" "<calcPr calcId=\"124519\" fullCalcOnLoad=\"1\"/>"
                        ]
        , test "cached results" <|
            \_ ->
                one
                    [ W.formulaWithResult "1+1" (W.int 2)
                    , W.formulaWithResult "\"a\"&\"b\"" (W.string "ab")
                    , W.formulaWithResult "1=1" (W.bool True)
                    , W.formulaWithResult "1/0" (W.error "#DIV/0!")
                    ]
                    |> Expect.all
                        [ contains sheet1 "<c r=\"A1\"><f>1+1</f><v>2</v></c><c r=\"B1\" t=\"str\"><f>\"a\"&amp;\"b\"</f><v>ab</v></c><c r=\"C1\" t=\"b\"><f>1=1</f><v>1</v></c><c r=\"D1\" t=\"e\"><f>1/0</f><v>#DIV/0!</v></c>"
                        , contains "xl/workbook.xml" "<calcPr calcId=\"124519\"/>"
                        ]
        , test "a date result gets a date format" <|
            \_ ->
                one [ W.formulaWithResult "DATE(2000,1,1)" (W.date (Date.fromCalendarDate 2000 Jan 1)) ]
                    |> contains sheet1 "<c r=\"A1\" s=\"1\"><f>DATE(2000,1,1)</f><v>36526</v></c>"
        , test "newer functions are prefixed" <|
            \_ ->
                one [ W.formula "XLOOKUP(1,A:A,B:B)" ]
                    |> contains sheet1 "<f>_xlfn.XLOOKUP(1,A:A,B:B)</f>"
        ]


validation : Test
validation =
    describe "validation"
        [ test "no sheets" <|
            \_ -> Expect.equal [ "A workbook needs at least one sheet." ] (problemsOf (W.workbook []))
        , test "bad sheet names" <|
            \_ ->
                W.workbook
                    [ W.sheet "" []
                    , W.sheet "a/b:c" []
                    , W.sheet "'quoted'" []
                    , W.sheet (String.repeat 32 "x") []
                    , W.sheet "history" []
                    ]
                    |> problemsOf
                    |> List.length
                    |> Expect.equal 5
        , test "duplicate names ignore case" <|
            \_ ->
                W.workbook [ W.sheet "Data" [], W.sheet "DATA" [] ]
                    |> problemsOf
                    |> Expect.equal [ "Sheet \"DATA\": Another sheet already has this name (names are compared ignoring case)." ]
        , test "unknown active sheet" <|
            \_ ->
                W.workbook [ W.sheet "A" [] ]
                    |> W.withActiveSheet "B"
                    |> problemsOf
                    |> Expect.equal [ "The active sheet \"B\" is not in the workbook." ]
        , test "every cell problem is reported with its location" <|
            \_ ->
                W.workbook
                    [ W.sheet "S"
                        [ W.row [ W.float (0 / 0), W.float (1 / 0) ]
                        , W.row [ W.date (Date.fromCalendarDate 1899 Dec 31), W.error "#OOPS", W.string (String.repeat 32768 "x"), W.formula "" ]
                        ]
                    ]
                    |> problemsOf
                    |> List.map (String.split ":" >> List.head >> Maybe.withDefault "")
                    |> Expect.equal [ "S!A1", "S!B1", "S!A2", "S!B2", "S!C2", "S!D2" ]
        , test "too many rows" <|
            \_ ->
                W.workbook [ W.sheet "S" [ W.emptyRows 1048576, W.row [ W.int 1 ], W.row [ W.int 2 ] ] ]
                    |> problemsOf
                    |> Expect.equal [ "Sheet \"S\": The sheet has more than 1048576 rows, Excel's limit." ]
        , test "too many columns" <|
            \_ ->
                W.workbook [ W.sheet "S" [ W.rowFrom 16384 [ W.int 1, W.int 2 ] ] ]
                    |> problemsOf
                    |> Expect.equal [ "Sheet \"S\": Row 1 has cells past column XFD, Excel's last column." ]
        , test "column below 1" <|
            \_ ->
                W.workbook [ W.sheet "S" [ W.rowFrom 0 [ W.int 1 ] ] ]
                    |> problemsOf
                    |> Expect.equal [ "Sheet \"S\": Row 1 starts at column 0; columns start at 1." ]
        ]


options : Test
options =
    describe "options"
        [ test "same input gives the same bytes" <|
            \_ ->
                let
                    wb =
                        one [ W.string "x", W.int 1 ]
                in
                Expect.equal (writeOk wb |> Result.map toList) (writeOk wb |> Result.map toList)
        , test "Store and Deflate produce readable packages" <|
            \_ ->
                [ W.Store, W.Deflate, W.Auto ]
                    |> List.map
                        (\c ->
                            W.toBytes { created = Time.millisToPosix 1794528000000, compression = c } (one [ W.string "x" ])
                                |> Result.toMaybe
                                |> Maybe.andThen (part "xl/sharedStrings.xml")
                                |> Maybe.map (String.contains "<t>x</t>")
                        )
                    |> Expect.equal [ Just True, Just True, Just True ]
        , test "creation time in core.xml" <|
            \_ ->
                W.toBytes { created = Time.millisToPosix 1794528000000, compression = W.Auto } (one [])
                    |> Result.toMaybe
                    |> Maybe.andThen (part "docProps/core.xml")
                    |> Maybe.map (String.contains "<dcterms:created xsi:type=\"dcterms:W3CDTF\">2026-11-13T00:00:00Z</dcterms:created>")
                    |> Expect.equal (Just True)
        ]


toList : Bytes.Bytes -> Maybe String
toList =
    Base64.fromBytes
