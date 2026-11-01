module EscapeTest exposing (suite)

import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)
import Xlsx.Internal.Xml.Escape as Escape


suite : Test
suite =
    describe "Xlsx.Internal.Xml.Escape"
        [ describe "text"
            [ test "markup characters" <| \_ -> Expect.equal "a &amp; b &lt;c&gt;" (Escape.text "a & b <c>")
            , test "quotes stay" <| \_ -> Expect.equal "\"hi\"" (Escape.text "\"hi\"")
            , test "already escaped entity is escaped again" <| \_ -> Expect.equal "&amp;amp;" (Escape.text "&amp;")
            ]
        , describe "attribute"
            [ test "quotes and newlines" <| \_ -> Expect.equal "&quot;a&quot;&#10;&amp;" (Escape.attribute "\"a\"\n&")
            ]
        , describe "encodeXString"
            [ test "plain text unchanged" <| \_ -> Expect.equal "Hello, world" (Escape.encodeXString "Hello, world")
            , test "control character" <| \_ -> Expect.equal "a_x0001_b" (Escape.encodeXString "a\u{0001}b")
            , test "vertical tab and form feed" <| \_ -> Expect.equal "_x000B__x000C_" (Escape.encodeXString "\u{000B}\u{000C}")
            , test "tab and newline are allowed" <| \_ -> Expect.equal "a\tb\nc" (Escape.encodeXString "a\tb\nc")
            , test "literal escape is protected" <| \_ -> Expect.equal "_x005F_x0041_" (Escape.encodeXString "_x0041_")
            , test "lowercase hex is protected too" <| \_ -> Expect.equal "_x005F_x00ff_" (Escape.encodeXString "_x00ff_")
            , test "almost an escape" <| \_ -> Expect.equal "_x004_ _xGGGG_" (Escape.encodeXString "_x004_ _xGGGG_")
            , test "emoji unchanged" <| \_ -> Expect.equal "👍🏽 ok" (Escape.encodeXString "👍🏽 ok")
            ]
        , describe "decodeXString"
            [ test "control character" <| \_ -> Expect.equal "a\u{0001}b" (Escape.decodeXString "a_x0001_b")
            , test "protected escape" <| \_ -> Expect.equal "_x0041_" (Escape.decodeXString "_x005F_x0041_")
            , test "uppercase and lowercase hex" <| \_ -> Expect.equal "AA" (Escape.decodeXString "_x0041__x0041_")
            , test "not an escape" <| \_ -> Expect.equal "_xZZ_" (Escape.decodeXString "_xZZ_")
            , fuzz Fuzz.string "decode undoes encode" <|
                \s -> Expect.equal s (Escape.decodeXString (Escape.encodeXString s))
            , fuzz (Fuzz.oneOfValues [ "_x0001_", "_x005F_", "x", "_", "a\u{0002}", "_x12", "AB_" ] |> Fuzz.listOfLengthBetween 0 8 |> Fuzz.map String.concat) "decode undoes encode on tricky input" <|
                \s -> Expect.equal s (Escape.decodeXString (Escape.encodeXString s))
            ]
        , describe "unescape"
            [ test "predefined entities" <| \_ -> Expect.equal "a & b <c> \"'" (Escape.unescape "a &amp; b &lt;c&gt; &quot;&apos;")
            , test "character references" <| \_ -> Expect.equal "AB€😀" (Escape.unescape "&#65;&#x42;&#8364;&#x1F600;")
            , test "unknown entity kept" <| \_ -> Expect.equal "&nbsp; &" (Escape.unescape "&nbsp; &")
            , test "double escaping decodes once" <| \_ -> Expect.equal "&amp;" (Escape.unescape "&amp;amp;")
            , fuzz Fuzz.string "undoes text escaping" <| \s -> Expect.equal s (Escape.unescape (Escape.text s))
            ]
        , describe "needsPreserve"
            [ test "plain" <| \_ -> Expect.equal False (Escape.needsPreserve "abc")
            , test "empty" <| \_ -> Expect.equal False (Escape.needsPreserve "")
            , test "leading space" <| \_ -> Expect.equal True (Escape.needsPreserve " abc")
            , test "trailing space" <| \_ -> Expect.equal True (Escape.needsPreserve "abc ")
            , test "inner newline" <| \_ -> Expect.equal True (Escape.needsPreserve "a\nb")
            , test "inner space" <| \_ -> Expect.equal False (Escape.needsPreserve "a b")
            , test "ends with an emoji (regression: used to hang)" <| \_ -> Expect.equal False (Escape.needsPreserve "rainbow 🌈")
            , test "emoji then trailing space" <| \_ -> Expect.equal True (Escape.needsPreserve "🌈 ")
            ]
        ]
