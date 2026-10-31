module TreeTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Xlsx.Internal.Xml.Tree as Tree


suite : Test
suite =
    describe "Xlsx.Internal.Xml.Tree"
        [ test "local names ignore prefixes, for elements and attributes" <|
            \_ ->
                Tree.parse "<x:workbook xmlns:x=\"main\" xmlns:r=\"rel\"><x:sheets><x:sheet name=\"A &amp; B\" r:id=\"rId1\"/></x:sheets></x:workbook>"
                    |> Result.map
                        (\root ->
                            ( Tree.name root
                            , Tree.child "sheets" root
                                |> Maybe.map (Tree.children "sheet")
                                |> Maybe.withDefault []
                                |> List.map (\s -> ( Tree.attribute "name" s, Tree.attribute "id" s ))
                            )
                        )
                    |> Expect.equal (Ok ( "workbook", [ ( Just "A & B", Just "rId1" ) ] ))
        , test "descendants and text" <|
            \_ ->
                Tree.parse "<si><r><t>Hello</t></r><r><t xml:space=\"preserve\"> world</t></r></si>"
                    |> Result.map (Tree.descendants "t" >> List.map Tree.text >> String.concat)
                    |> Expect.equal (Ok "Hello world")
        , test "DOCTYPE is rejected" <|
            \_ ->
                Tree.parse "<?xml version=\"1.0\"?><!DOCTYPE lol [<!ENTITY a \"aaaa\">]><r>&a;</r>"
                    |> Expect.err
        , test "malformed XML" <|
            \_ -> Tree.parse "<a><b></a>" |> Expect.err
        ]
