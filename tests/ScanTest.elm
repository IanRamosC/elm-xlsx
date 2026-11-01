module ScanTest exposing (suite)

import Array
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)
import Xlsx.Internal.SharedStrings as SharedStrings
import Xlsx.Internal.Xml.Scan as Scan


suite : Test
suite =
    describe "Xlsx.Internal.Xml.Scan"
        [ describe "rootPrefix"
            [ test "no prefix" <| \_ -> Expect.equal "" (Scan.rootPrefix "<?xml version=\"1.0\"?>\n<worksheet xmlns=\"main\"/>")
            , test "x: prefix after a comment" <| \_ -> Expect.equal "x:" (Scan.rootPrefix "<?xml version=\"1.0\"?><!-- hi --><x:worksheet xmlns:x=\"main\"/>")
            ]
        , describe "elements"
            [ test "splits and skips longer names" <|
                \_ ->
                    Scan.elements "" "c" "<row><c r=\"A1\"><v>1</v></c><cols/><c r=\"B1\"/></row>"
                        |> Expect.equal [ " r=\"A1\"><v>1</v></c><cols/>", " r=\"B1\"/></row>" ]
            , test "prefix" <|
                \_ ->
                    Scan.elements "x:" "c" "<x:row><x:c r=\"A1\"/><x:c\tr=\"B1\"/></x:row>"
                        |> List.length
                        |> Expect.equal 2
            ]
        , describe "attribute"
            [ test "double quotes" <| \_ -> Expect.equal (Just "A1") (Scan.attribute "r" " r=\"A1\" t=\"s\"")
            , test "single quotes" <| \_ -> Expect.equal (Just "s") (Scan.attribute "t" " r='A1' t='s'")
            , test "not confused by a longer name" <| \_ -> Expect.equal Nothing (Scan.attribute "s" " ss=\"1\"")
            , test "newline separator" <| \_ -> Expect.equal (Just "3") (Scan.attribute "s" " r=\"A1\"\ns=\"3\"")
            , test "unescaped" <| \_ -> Expect.equal (Just "a&b") (Scan.attribute "n" " n=\"a&amp;b\"")
            ]
        , describe "content"
            [ test "value" <| \_ -> Expect.equal (Just "42") (Scan.content "" "v" " r=\"A1\"><v>42</v></c>")
            , test "self-closing" <| \_ -> Expect.equal (Just "") (Scan.content "" "f" "><f t=\"shared\" si=\"0\"/><v>1</v></c>")
            , test "absent" <| \_ -> Expect.equal Nothing (Scan.content "" "f" "><v>1</v></c>")
            ]
        , describe "decodeText"
            [ test "entities and escapes" <| \_ -> Expect.equal "a<b\u{0001}" (Scan.decodeText "a&lt;b_x0001_")
            , test "CRLF becomes LF but &#13; survives" <| \_ -> Expect.equal "a\nb\u{000D}" (Scan.decodeText "a\u{000D}\nb&#13;")
            , test "CDATA is literal" <| \_ -> Expect.equal "<b>&amp; ok" (Scan.decodeText "<![CDATA[<b>&amp;]]> ok")
            ]
        , describe "SharedStrings.parse"
            [ test "plain, rich and phonetic" <|
                \_ ->
                    SharedStrings.parse
                        ("<sst xmlns=\"main\" count=\"4\" uniqueCount=\"4\">"
                            ++ "<si><t>Hello</t></si>"
                            ++ "<si><r><rPr><b/><sz val=\"11\"/></rPr><t>Bold</t></r><r><t xml:space=\"preserve\"> and plain</t></r></si>"
                            ++ "<si><t>東京</t><rPh sb=\"0\" eb=\"2\"><t>トウキョウ</t></rPh><phoneticPr fontId=\"1\"/></si>"
                            ++ "<si><t/></si><si/>"
                            ++ "</sst>"
                        )
                        |> Array.toList
                        |> Expect.equal [ "Hello", "Bold and plain", "東京", "", "" ]
            , test "prefixed" <|
                \_ ->
                    SharedStrings.parse "<x:sst xmlns:x=\"main\"><x:si><x:t>a &amp; b</x:t></x:si></x:sst>"
                        |> Array.toList
                        |> Expect.equal [ "a & b" ]
            , fuzz Fuzz.string "round-trips what the writer produces" <|
                \s ->
                    SharedStrings.empty
                        |> SharedStrings.insert s
                        |> Tuple.second
                        |> SharedStrings.toXml
                        |> SharedStrings.parse
                        |> Array.toList
                        |> Expect.equal [ String.replace "\u{000D}" "\n" (String.replace "\u{000D}\n" "\n" s) ]
            ]
        ]
