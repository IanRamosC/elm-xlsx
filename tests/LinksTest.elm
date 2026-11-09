module LinksTest exposing (suite)

import Expect
import Helpers exposing (contains, containsAll, part, partNames, writeOk)
import Test exposing (Test, describe, test)
import Time
import Xlsx.Internal.SheetWriter as SheetWriter
import Xlsx.Read as Read exposing (CellValue(..))
import Xlsx.Style as Style
import Xlsx.Write as W


problems : W.Workbook -> List String
problems wb =
    case W.toBytes W.defaultOptions wb of
        Ok _ ->
            []

        Err ps ->
            List.map W.problemToString ps


readBack : W.Workbook -> Result String (List ( String, Maybe String ))
readBack wb =
    W.toBytes { created = Time.millisToPosix 0, compression = W.Store } wb
        |> Result.mapError (List.map W.problemToString >> String.join "; ")
        |> Result.andThen (Read.read >> Result.andThen Read.firstSheet >> Result.mapError Read.errorToString)
        |> Result.map (Read.cells >> List.map (\( ref, c ) -> ( Debug.toString c.value, c.hyperlink )))


links : W.Workbook
links =
    W.workbook
        [ W.sheet "Links"
            [ W.row
                [ W.hyperlink "https://example.com/a b" "Example"
                , W.int 42 |> W.linkTo "mailto:ada@example.com"
                , W.string "Totals" |> W.linkToCell "'Other sheet'!B2"
                , W.string "plain"
                ]
            ]
        , W.sheet "Other sheet" []
        ]


suite : Test
suite =
    describe "hyperlinks and document properties"
        [ describe "writing links"
            [ test "hyperlinks element and relationships" <|
                \_ ->
                    links
                        |> containsAll "xl/worksheets/sheet1.xml"
                            [ "</sheetData><hyperlinks><hyperlink ref=\"A1\" r:id=\"rId1\"/><hyperlink ref=\"B1\" r:id=\"rId2\"/>"
                            , "<hyperlink ref=\"C1\" location=\"'Other sheet'!B2\" display=\"'Other sheet'!B2\"/></hyperlinks><pageMargins"
                            ]
            , test "external targets, with spaces escaped" <|
                \_ ->
                    links
                        |> containsAll "xl/worksheets/_rels/sheet1.xml.rels"
                            [ "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink\" Target=\"https://example.com/a%20b\" TargetMode=\"External\"/>"
                            , "Target=\"mailto:ada@example.com\" TargetMode=\"External\"/>"
                            ]
            , test "sheets without external links get no relationships part" <|
                \_ ->
                    writeOk links
                        |> Result.map (partNames >> List.filter (String.contains "_rels/sheet"))
                        |> Expect.equal (Ok [ "xl/worksheets/_rels/sheet1.xml.rels" ])
            , test "the Hyperlink cell style" <|
                \_ ->
                    links
                        |> containsAll "xl/styles.xml"
                            [ "<font><u/><sz val=\"11\"/><color rgb=\"FF0563C1\"/><name val=\"Calibri\"/><family val=\"2\"/></font>"
                            , "<cellStyleXfs count=\"2\">"
                            , "xfId=\"1\" applyAlignment=\"1\" applyProtection=\"1\"/>"
                            , "<cellStyle name=\"Hyperlink\" xfId=\"1\" builtinId=\"8\"/>"
                            ]
            , test "own styles layer over the link style" <|
                \_ ->
                    W.workbook [ W.sheet "S" [ W.row [ W.hyperlink "https://x.org" "x" |> W.withStyle (Style.bold Style.none) ] ] ]
                        |> contains "xl/styles.xml" "<font><b/><u/><sz val=\"11\"/><color rgb=\"FF0563C1\"/>"
            , test "no Hyperlink style without links" <|
                \_ ->
                    W.workbook [ W.sheet "S" [ W.row [ W.string "x" ] ] ]
                        |> writeOk
                        |> Result.map (part "xl/styles.xml" >> Maybe.map (String.contains "Hyperlink"))
                        |> Expect.equal (Ok (Just False))
            , test "problems: unknown sheet, empty and too-long URLs" <|
                \_ ->
                    W.workbook
                        [ W.sheet "S"
                            [ W.row
                                [ W.string "a" |> W.linkToCell "Nope!A1"
                                , W.string "b" |> W.linkTo " "
                                , W.string "c" |> W.linkTo ("https://x.org/" ++ String.repeat 2100 "a")
                                , W.string "d" |> W.linkToCell "s!A1"
                                , W.string "e" |> W.linkToCell "#A5"
                                ]
                            ]
                        ]
                        |> problems
                        |> Expect.equal
                            [ "S!A1: The link goes to the sheet \"Nope\", which is not in the workbook."
                            , "S!B1: The link has an empty URL."
                            , "S!C1: The link's URL has 2114 characters; Excel allows 2079."
                            ]
            , test "URL escaping" <|
                \_ ->
                    List.map SheetWriter.escapeUrl [ "https://x.org/a b?q=\"1\"&r=<2>", "https://x.org/100%", "https://x.org/%20ok", "https://例え.jp/ü" ]
                        |> Expect.equal [ "https://x.org/a%20b?q=%221%22&r=%3C2%3E", "https://x.org/100%25", "https://x.org/%20ok", "https://例え.jp/ü" ]
            ]
        , test "links read back" <|
            \_ ->
                readBack links
                    |> Expect.equal
                        (Ok
                            [ ( "Text \"Example\"", Just "https://example.com/a%20b" )
                            , ( "Number 42", Just "mailto:ada@example.com" )
                            , ( "Text \"Totals\"", Just "#'Other sheet'!B2" )
                            , ( "Text \"plain\"", Nothing )
                            ]
                        )
        , describe "document properties"
            [ test "core and app parts" <|
                \_ ->
                    W.workbook [ W.sheet "S" [] ]
                        |> W.withProperties
                            [ W.title "Q3 & Q4"
                            , W.subject "Sales"
                            , W.author "Ada"
                            , W.manager "Grace"
                            , W.company "Acme"
                            , W.category "Reports"
                            , W.keywords "sales, 2026"
                            , W.comments "Generated"
                            , W.status "Final"
                            ]
                        |> Expect.all
                            [ containsAll "docProps/core.xml"
                                [ "<dc:title>Q3 &amp; Q4</dc:title><dc:subject>Sales</dc:subject><dc:creator>Ada</dc:creator><cp:keywords>sales, 2026</cp:keywords><dc:description>Generated</dc:description><cp:lastModifiedBy>Ada</cp:lastModifiedBy>"
                                , "</dcterms:modified><cp:category>Reports</cp:category><cp:contentStatus>Final</cp:contentStatus></cp:coreProperties>"
                                ]
                            , contains "docProps/app.xml" "<Manager>Grace</Manager><Company>Acme</Company>"
                            ]
            , test "empty properties are left out" <|
                \_ ->
                    W.workbook [ W.sheet "S" [] ]
                        |> contains "docProps/core.xml" "<dc:creator></dc:creator><cp:lastModifiedBy></cp:lastModifiedBy>"
            ]
        ]
