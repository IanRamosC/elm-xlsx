module Xlsx.Internal.Xml.Escape exposing
    ( text, attribute
    , encodeXString, decodeXString
    , needsPreserve
    , unescape
    )

{-| Escaping for the XML we write.

Two layers are involved for cell text:

1.  `encodeXString` applies OOXML's own `_xHHHH_` escaping (ECMA-376
    `ST_Xstring`) so characters that XML 1.0 forbids, like U+0001, survive.
    A literal `_xHHHH_` already in the text is protected by escaping its
    underscore as `_x005F_`.
2.  `text` / `attribute` apply ordinary XML escaping.

@docs text, attribute
@docs encodeXString, decodeXString
@docs needsPreserve
@docs unescape

-}


{-| Escape `&`, `<` and `>` for element content.
-}
text : String -> String
text s =
    if String.any (\c -> c == '&' || c == '<' || c == '>') s then
        s
            |> String.replace "&" "&amp;"
            |> String.replace "<" "&lt;"
            |> String.replace ">" "&gt;"

    else
        s


{-| Escape for a double-quoted attribute value. Tabs and newlines become
character references so attribute normalization does not turn them into
spaces.
-}
attribute : String -> String
attribute s =
    if String.any (\c -> c == '&' || c == '<' || c == '>' || c == '"' || c == '\n' || c == '\u{000D}' || c == '\t') s then
        s
            |> String.replace "&" "&amp;"
            |> String.replace "<" "&lt;"
            |> String.replace ">" "&gt;"
            |> String.replace "\"" "&quot;"
            |> String.replace "\n" "&#10;"
            |> String.replace "\u{000D}" "&#13;"
            |> String.replace "\t" "&#9;"

    else
        s


{-| Apply `_xHHHH_` escaping to characters that are not allowed in XML 1.0,
and protect literal `_xHHHH_` sequences.

    encodeXString "a\u{0001}b" == "a_x0001_b"

    encodeXString "_x0041_" == "_x005F_x0041_"

-}
encodeXString : String -> String
encodeXString s =
    if String.any isForbidden s || String.contains "_x" s then
        String.foldr encodeChar ( [], [] ) s
            |> (\( acc, _ ) -> String.concat acc)

    else
        s


{-| Walk the string right to left. `ahead` holds the next 6 characters, which
is all we need to spot a `_xHHHH_` pattern that starts at the current `_`.
-}
encodeChar : Char -> ( List String, List Char ) -> ( List String, List Char )
encodeChar c ( acc, ahead ) =
    let
        nextAhead =
            List.take 6 (c :: ahead)
    in
    if isForbidden c then
        ( ("_x" ++ hex4 (Char.toCode c) ++ "_") :: acc, nextAhead )

    else if c == '_' && startsEscape ahead then
        ( "_x005F_" :: acc, nextAhead )

    else
        ( String.fromChar c :: acc, nextAhead )


startsEscape : List Char -> Bool
startsEscape ahead =
    case ahead of
        [ 'x', a, b, c, d, '_' ] ->
            List.all Char.isHexDigit [ a, b, c, d ]

        _ ->
            False


{-| Undo `_xHHHH_` escaping, as done when reading shared strings and inline
strings.

    decodeXString "a_x0001_b" == "a\u{0001}b"

    decodeXString "_x005F_x0041_" == "_x0041_"

-}
decodeXString : String -> String
decodeXString s =
    if String.contains "_x" s then
        decodeHelp (String.toList s) []

    else
        s


decodeHelp : List Char -> List String -> String
decodeHelp chars acc =
    case chars of
        '_' :: 'x' :: a :: b :: c :: d :: '_' :: rest ->
            case hexToInt [ a, b, c, d ] of
                Just code ->
                    decodeHelp rest (String.fromChar (Char.fromCode code) :: acc)

                Nothing ->
                    decodeHelp ('x' :: a :: b :: c :: d :: '_' :: rest) ("_" :: acc)

        c :: rest ->
            decodeHelp rest (String.fromChar c :: acc)

        [] ->
            String.concat (List.reverse acc)


{-| Whether a `<t>` element needs `xml:space="preserve"`: leading or trailing
whitespace, or a line break or tab anywhere.
-}
needsPreserve : String -> Bool
needsPreserve s =
    case String.uncons s of
        Just ( first, _ ) ->
            -- `String.right 1` would split an emoji's surrogate pair, and
            -- elm/core's `String.any` never terminates on a lone surrogate.
            isSpace first || String.endsWith " " s || String.any (\c -> c == '\n' || c == '\t' || c == '\u{000D}') s

        Nothing ->
            False


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}'


{-| Characters XML 1.0 does not allow, apart from tab, line feed and
carriage return.
-}
isForbidden : Char -> Bool
isForbidden c =
    let
        code =
            Char.toCode c
    in
    (code < 0x20 && code /= 0x09 && code /= 0x0A && code /= 0x0D)
        || code
        == 0xFFFE
        || code
        == 0xFFFF


hex4 : Int -> String
hex4 n =
    String.fromList (List.map (\shift -> hexDigit (modBy 16 (n // shift))) [ 4096, 256, 16, 1 ])


hexDigit : Int -> Char
hexDigit d =
    if d < 10 then
        Char.fromCode (48 + d)

    else
        Char.fromCode (55 + d)


hexToInt : List Char -> Maybe Int
hexToInt =
    List.foldl
        (\c acc ->
            Maybe.andThen
                (\total ->
                    let
                        code =
                            Char.toCode (Char.toUpper c)
                    in
                    if code >= 48 && code <= 57 then
                        Just (total * 16 + code - 48)

                    else if code >= 65 && code <= 70 then
                        Just (total * 16 + code - 55)

                    else
                        Nothing
                )
                acc
        )
        (Just 0)


{-| Decode XML character references and the five predefined entities, for
text read without an XML parser.

    unescape "a &amp; b &lt;c&gt; &#65;&#x42;" == "a & b <c> AB"

Unknown entities are left as they are.

-}
unescape : String -> String
unescape s =
    if String.contains "&" s then
        case String.split "&" s of
            first :: rest ->
                String.concat (first :: List.map unescapeChunk rest)

            [] ->
                s

    else
        s


{-| A chunk that followed an `&`: decode the entity at its start.
-}
unescapeChunk : String -> String
unescapeChunk chunk =
    case String.indexes ";" (String.left 12 chunk) of
        end :: _ ->
            let
                entity =
                    String.left end chunk
            in
            case entityChar entity of
                Just decoded ->
                    let
                        rest =
                            String.dropLeft (end + 1) chunk
                    in
                    decoded ++ rest

                Nothing ->
                    "&" ++ chunk

        [] ->
            "&" ++ chunk


entityChar : String -> Maybe String
entityChar entity =
    case entity of
        "amp" ->
            Just "&"

        "lt" ->
            Just "<"

        "gt" ->
            Just ">"

        "quot" ->
            Just "\""

        "apos" ->
            Just "'"

        _ ->
            if String.startsWith "#x" entity || String.startsWith "#X" entity then
                hexToInt (String.toList (String.dropLeft 2 entity)) |> Maybe.andThen codePoint

            else if String.startsWith "#" entity then
                String.toInt (String.dropLeft 1 entity) |> Maybe.andThen codePoint

            else
                Nothing


codePoint : Int -> Maybe String
codePoint code =
    if code > 0 && code <= 0x0010FFFF then
        Just (String.fromChar (Char.fromCode code))

    else
        Nothing
