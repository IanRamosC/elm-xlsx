module ReadTest exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Date
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)
import Time exposing (Month(..))
import Xlsx.Date as XDate
import Xlsx.Read as Read exposing (CellValue(..))
import Xlsx.Ref as Ref
import Xlsx.Style as Style
import Xlsx.Write as Write
import Zip
import Zip.Entry as Entry


roundTrip : List Write.Sheet -> Result Read.Error Read.Workbook
roundTrip sheets =
    case Write.toBytes { created = Time.millisToPosix 0, compression = Write.Store } (Write.workbook sheets) of
        Ok bytes ->
            Read.read bytes

        Err problems ->
            Err (Read.UnsupportedFormat (String.join "; " (List.map Write.problemToString problems)))


firstRows : List Write.Sheet -> Result Read.Error (List (List CellValue))
firstRows sheets =
    roundTrip sheets |> Result.andThen Read.firstSheet |> Result.map Read.rows


{-| A package from raw parts, for files our writer would never produce.
-}
package : List ( String, String ) -> Bytes
package parts =
    parts
        |> List.map
            (\( path, text ) ->
                Entry.store
                    { path = path, lastModified = ( Time.utc, Time.millisToPosix 315532800000 ), comment = Nothing }
                    (Encode.encode (Encode.string text))
            )
        |> Zip.fromEntries
        |> Zip.toBytes


minimalParts : String -> String -> List ( String, String )
minimalParts workbookXml sheetXml =
    [ ( "[Content_Types].xml", "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"/>" )
    , ( "_rels/.rels", "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/></Relationships>" )
    , ( "xl/workbook.xml", workbookXml )
    , ( "xl/_rels/workbook.xml.rels", "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet1.xml\"/><Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/chartsheet\" Target=\"/xl/chartsheets/sheet1.xml\"/></Relationships>" )
    , ( "xl/worksheets/sheet1.xml", sheetXml )
    ]


suite : Test
suite =
    describe "Xlsx.Read"
        [ describe "round trip through Xlsx.Write"
            [ test "values of every type" <|
                \_ ->
                    firstRows
                        [ Write.sheet "Data"
                            [ Write.row [ Write.string "Name", Write.int 42, Write.float 2.5, Write.bool True, Write.error "#N/A" ]
                            , Write.row [ Write.date (Date.fromCalendarDate 2024 Feb 29), Write.dateTime (XDate.dateTime (Date.fromCalendarDate 2024 Feb 29) 13 45 30), Write.blank, Write.string "x" ]
                            ]
                        ]
                        |> Expect.equal
                            (Ok
                                [ [ Text "Name", Number 42, Number 2.5, Boolean True, ErrorValue "#N/A" ]
                                , [ DateValue (Date.fromCalendarDate 2024 Feb 29), DateTimeValue (XDate.dateTime (Date.fromCalendarDate 2024 Feb 29) 13 45 30), Empty, Text "x" ]
                                ]
                            )
            , test "gaps are padded, leading empty rows kept" <|
                \_ ->
                    firstRows [ Write.sheet "S" [ Write.emptyRow, Write.rowFrom 3 [ Write.int 1 ] ] ]
                        |> Expect.equal (Ok [ [], [ Empty, Empty, Number 1 ] ])
            , test "formulas, number formats and dimensions" <|
                \_ ->
                    roundTrip
                        [ Write.sheet "S"
                            [ Write.row [ Write.int 2, Write.formulaWithResult "A1*2" (Write.int 4), Write.formula "XLOOKUP(1,A:A,B:B)" ]
                            , Write.row [ Write.float 0.25 |> Write.withStyle (Style.numberFormat (Style.percent 0) Style.none) ]
                            ]
                        ]
                        |> Result.andThen Read.firstSheet
                        |> Result.map
                            (\s ->
                                ( List.map (\r -> Read.cell r s) [ { column = 2, row = 1 }, { column = 3, row = 1 }, { column = 1, row = 2 } ]
                                , Read.dimensions s |> Maybe.map Ref.rangeToString
                                )
                            )
                        |> Expect.equal
                            (Ok
                                ( [ { value = Number 4, formula = Just "A1*2", numberFormat = "General" }
                                  , { value = Empty, formula = Just "XLOOKUP(1,A:A,B:B)", numberFormat = "General" }
                                  , { value = Number 0.25, formula = Nothing, numberFormat = "0%" }
                                  ]
                                , Just "A1:C2"
                                )
                            )
            , test "sheets, lookup ignoring case, merges" <|
                \_ ->
                    roundTrip
                        [ Write.sheet "One" [ Write.row [ Write.string "a" ] ]
                        , Write.sheet "Two" [ Write.row [ Write.string "b" ] ] |> Write.merge "A1:B2"
                        ]
                        |> Result.andThen
                            (\wb ->
                                Read.sheet "two" wb
                                    |> Result.map (\s -> ( Read.sheetNames wb, Read.name s, List.map Ref.rangeToString (Read.merges s) ))
                            )
                        |> Expect.equal (Ok ( [ "One", "Two" ], "Two", [ "A1:B2" ] ))
            , test "unknown sheet" <|
                \_ ->
                    roundTrip [ Write.sheet "One" [] ]
                        |> Result.andThen (Read.sheet "Nope")
                        |> Result.map (always ())
                        |> Expect.equal (Err (Read.SheetNotFound "Nope"))
            , test "empty sheet" <|
                \_ ->
                    roundTrip [ Write.sheet "One" [] ]
                        |> Result.andThen Read.firstSheet
                        |> Result.map (\s -> ( Read.rows s, Read.dimensions s ))
                        |> Expect.equal (Ok ( [], Nothing ))
            , fuzz Fuzz.string "any text survives" <|
                \s ->
                    firstRows [ Write.sheet "S" [ Write.row [ Write.string s ] ] ]
                        |> Expect.equal (Ok [ [ Text (String.replace "\u{000D}" "\n" (String.replace "\u{000D}\n" "\n" s)) ] ])
            , fuzz (Fuzz.floatRange -1.0e300 1.0e300) "any number survives" <|
                \n ->
                    firstRows [ Write.sheet "S" [ Write.row [ Write.float n ] ] ]
                        |> Expect.equal (Ok [ [ Number n ] ])
            ]
        , describe "files from other writers"
            [ test "x: prefixes, 1904 dates, hidden sheets, chart sheets, inline strings" <|
                \_ ->
                    package
                        (minimalParts
                            ("<x:workbook xmlns:x=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><x:workbookPr date1904=\"1\"/><x:sheets>"
                                ++ "<x:sheet name=\"Data\" sheetId=\"1\" state=\"hidden\" r:id=\"rId1\"/><x:sheet name=\"Chart\" sheetId=\"2\" r:id=\"rId2\"/></x:sheets></x:workbook>"
                            )
                            "<x:worksheet xmlns:x=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><x:sheetData><x:row><x:c t=\"inlineStr\"><x:is><x:t>hi</x:t></x:is></x:c><x:c t=\"d\"><x:v>2020-01-02T03:04:05.5Z</x:v></x:c></x:row></x:sheetData></x:worksheet>"
                        )
                        |> Read.read
                        |> Result.andThen
                            (\wb ->
                                Result.map
                                    (\s -> ( Read.sheets wb, Read.rows s ))
                                    (Read.sheet "Data" wb)
                            )
                        |> Expect.equal
                            (Ok
                                ( [ { name = "Data", visibility = Read.Hidden, kind = Read.Worksheet }
                                  , { name = "Chart", visibility = Read.Visible, kind = Read.Chartsheet }
                                  ]
                                , [ [ Text "hi", DateTimeValue { date = Date.fromCalendarDate 2020 Jan 2, hour = 3, minute = 4, second = 5, millisecond = 500 } ] ]
                                )
                            )
            , test "1904 serials" <|
                \_ ->
                    package
                        (( "xl/styles.xml", "<styleSheet><cellXfs><xf numFmtId=\"0\"/><xf numFmtId=\"14\"/></cellXfs></styleSheet>" )
                            :: List.map
                                (\( p, t ) ->
                                    if p == "xl/_rels/workbook.xml.rels" then
                                        ( p, "<Relationships><Relationship Id=\"rId1\" Type=\"http://purl.oclc.org/ooxml/officeDocument/relationships/worksheet\" Target=\"worksheets/sheet1.xml\"/><Relationship Id=\"rId3\" Type=\"http://purl.oclc.org/ooxml/officeDocument/relationships/styles\" Target=\"styles.xml\"/></Relationships>" )

                                    else
                                        ( p, t )
                                )
                                (minimalParts
                                    "<workbook><workbookPr date1904=\"true\"/><sheets><sheet name=\"S\" sheetId=\"1\" r:id=\"rId1\" xmlns:r=\"r\"/></sheets></workbook>"
                                    "<worksheet><sheetData><row r=\"1\"><c r=\"A1\" s=\"1\"><v>0</v></c><c r=\"B1\" s=\"1\"><v>1461</v></c></row></sheetData></worksheet>"
                                )
                        )
                        |> Read.read
                        |> Result.andThen Read.firstSheet
                        |> Result.map Read.rows
                        |> Expect.equal (Ok [ [ DateValue (Date.fromCalendarDate 1904 Jan 1), DateValue (Date.fromCalendarDate 1908 Jan 1) ] ])
            , test "chart sheets have no cells" <|
                \_ ->
                    package
                        (minimalParts
                            "<workbook><sheets><sheet name=\"S\" sheetId=\"1\" r:id=\"rId1\" xmlns:r=\"r\"/><sheet name=\"C\" sheetId=\"2\" r:id=\"rId2\" xmlns:r=\"r\"/></sheets></workbook>"
                            "<worksheet><sheetData/></worksheet>"
                        )
                        |> Read.read
                        |> Result.andThen (Read.sheet "C")
                        |> Result.map (always ())
                        |> isErr
                        |> Expect.equal True
            ]
        , describe "refusals"
            [ test "not a zip" <| \_ -> Read.read (Encode.encode (Encode.string "name,age\nAda,36\n")) |> Result.map (always ()) |> Expect.equal (Err Read.NotAnXlsxFile)
            , test "a zip without a workbook" <| \_ -> Read.read (package [ ( "word/document.xml", "<w:document/>" ) ]) |> Result.map (always ()) |> Expect.equal (Err Read.NotAnXlsxFile)
            , test "legacy or encrypted" <|
                \_ ->
                    Read.read (Encode.encode (Encode.sequence (List.map Encode.unsignedInt8 [ 0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1, 0, 0 ])))
                        |> Result.map (always ())
                        |> Expect.equal (Err Read.EncryptedOrLegacyXls)
            , test "xlsb" <| \_ -> Read.read (package [ ( "xl/workbook.bin", "" ) ]) |> Result.map (always ()) |> isErr |> Expect.equal True
            , test "DOCTYPE in a worksheet" <|
                \_ ->
                    package
                        (minimalParts
                            "<workbook><sheets><sheet name=\"S\" sheetId=\"1\" r:id=\"rId1\" xmlns:r=\"r\"/></sheets></workbook>"
                            "<?xml version=\"1.0\"?><!DOCTYPE x [<!ENTITY a \"aaaa\">]><worksheet><sheetData/></worksheet>"
                        )
                        |> Read.read
                        |> Result.andThen Read.firstSheet
                        |> Result.map (always ())
                        |> isErr
                        |> Expect.equal True
            ]
        ]


isErr : Result e a -> Bool
isErr r =
    case r of
        Err _ ->
            True

        Ok _ ->
            False
