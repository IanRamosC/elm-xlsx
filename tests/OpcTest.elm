module OpcTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Xlsx.Internal.Opc as Opc


suite : Test
suite =
    describe "Xlsx.Internal.Opc reading"
        [ describe "resolveTarget"
            [ test "relative" <| \_ -> Expect.equal "xl/worksheets/sheet1.xml" (Opc.resolveTarget "xl/workbook.xml" "worksheets/sheet1.xml")
            , test "absolute" <| \_ -> Expect.equal "xl/styles.xml" (Opc.resolveTarget "xl/workbook.xml" "/xl/styles.xml")
            , test "parent folder" <| \_ -> Expect.equal "xl/drawings/drawing1.xml" (Opc.resolveTarget "xl/worksheets/sheet1.xml" "../drawings/drawing1.xml")
            , test "from the package root" <| \_ -> Expect.equal "xl/workbook.xml" (Opc.resolveTarget "" "xl/workbook.xml")
            , test "percent-encoded" <| \_ -> Expect.equal "xl/worksheets/my sheet.xml" (Opc.resolveTarget "xl/workbook.xml" "worksheets/my%20sheet.xml")
            , test "dot segments" <| \_ -> Expect.equal "xl/a.xml" (Opc.resolveTarget "xl/workbook.xml" "./b/../a.xml")
            ]
        , describe "relsPathFor"
            [ test "workbook" <| \_ -> Expect.equal "xl/_rels/workbook.xml.rels" (Opc.relsPathFor "xl/workbook.xml")
            , test "package" <| \_ -> Expect.equal "_rels/.rels" (Opc.relsPathFor "")
            ]
        , test "parseRelationships" <|
            \_ ->
                Opc.parseRelationships "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://x/worksheet\" Target=\"worksheets/sheet1.xml\"/><Relationship Id=\"rId2\" Type=\"http://x/hyperlink\" Target=\"https://example.com/?a=1&amp;b=2\" TargetMode=\"External\"/></Relationships>"
                    |> Expect.equal
                        (Ok
                            [ { id = "rId1", type_ = "http://x/worksheet", target = "worksheets/sheet1.xml", external = False }
                            , { id = "rId2", type_ = "http://x/hyperlink", target = "https://example.com/?a=1&b=2", external = True }
                            ]
                        )
        , test "isRelType matches Transitional and Strict" <|
            \_ ->
                Expect.equal [ True, True, False ]
                    [ Opc.isRelType "officeDocument" "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
                    , Opc.isRelType "officeDocument" "http://purl.oclc.org/ooxml/officeDocument/relationships/officeDocument"
                    , Opc.isRelType "sheet" "http://x/worksheet"
                    ]
        ]
