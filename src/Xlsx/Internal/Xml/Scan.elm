module Xlsx.Internal.Xml.Scan exposing
    ( document, rootPrefix, hasDoctype
    , elements, section, startTag, attribute, prefixedAttribute, content
    , textRuns, decodeText
    )

{-| A fast, forgiving reader for every XML part. It works with
`String.split`, which was 4× faster than `elm/parser` in the M0 benchmark,
and takes time proportional to the size of the part. (A DOM parser was tried
for the small parts and took minutes on a 7 MB `styles.xml`.)

It does not validate XML: it finds the elements and attributes it is asked
for and ignores the rest. Documents with a `<!DOCTYPE` are refused, which
rules out entity-expansion attacks.

Every function takes the namespace prefix of the document (`""` or `"x:"`),
found once with `rootPrefix`.

@docs document, rootPrefix, hasDoctype
@docs elements, section, startTag, attribute, prefixedAttribute, content
@docs textRuns, decodeText

-}

import Xlsx.Internal.Xml.Escape as Escape


{-| Check a part before scanning it: refuse a DOCTYPE, and require a root
element with the expected local name. Gives back the namespace prefix.

    document "workbook" "<x:workbook xmlns:x=\"...\">...</x:workbook>" == Ok "x:"

-}
document : String -> String -> Result String String
document expectedRoot xml =
    if hasDoctype xml then
        Err "it has a DOCTYPE, which spreadsheets never use"

    else
        let
            prefix =
                rootPrefix xml
        in
        case elements prefix expectedRoot xml of
            _ :: _ ->
                Ok prefix

            [] ->
                Err ("it has no <" ++ expectedRoot ++ "> element")


{-| XML requires `DOCTYPE` in capitals, but some parsers accept any case.
-}
hasDoctype : String -> Bool
hasDoctype xml =
    String.contains "<!DOCTYPE" xml || String.contains "<!doctype" xml


{-| The prefix of the root element, colon included: `"x:"` for
`<x:worksheet ...>`, `""` for `<worksheet ...>`. Skips the XML declaration,
comments and processing instructions.
-}
rootPrefix : String -> String
rootPrefix xml =
    case String.split "<" xml |> List.filter isElementStart |> List.head of
        Just chunk ->
            let
                tagName =
                    chunk
                        |> String.toList
                        |> takeWhile (\c -> not (isNameEnd c))
                        |> String.fromList
            in
            case String.indexes ":" tagName of
                i :: _ ->
                    String.left (i + 1) tagName

                [] ->
                    ""

        Nothing ->
            ""


isElementStart : String -> Bool
isElementStart chunk =
    case String.uncons chunk of
        Just ( c, _ ) ->
            c /= '?' && c /= '!' && c /= '/'

        Nothing ->
            False


takeWhile : (a -> Bool) -> List a -> List a
takeWhile keep list =
    case list of
        x :: rest ->
            if keep x then
                x :: takeWhile keep rest

            else
                []

        [] ->
            []


isNameEnd : Char -> Bool
isNameEnd c =
    c == ' ' || c == '>' || c == '/' || c == '\t' || c == '\n' || c == '\u{000D}'


{-| The elements named `prefix ++ name`, each as the text that follows its
name (attributes, `>`, content, and everything up to the next element of the
same name). Chunks where the name is only the start of a longer name
(`<c` in `<cols`) are merged back into the previous element.

    elements "" "c" "<row><c r=\"A1\"><v>1</v></c><c r=\"B1\"/></row>"
        == [ " r=\"A1\"><v>1</v></c>", " r=\"B1\"/></row>" ]

-}
elements : String -> String -> String -> List String
elements prefix name xml =
    case String.split ("<" ++ prefix ++ name) xml of
        _ :: chunks ->
            mergeFalseStarts ("<" ++ prefix ++ name) chunks

        [] ->
            []


mergeFalseStarts : String -> List String -> List String
mergeFalseStarts separator chunks =
    List.foldl
        (\chunk acc ->
            if endsName chunk then
                chunk :: acc

            else
                case acc of
                    previous :: rest ->
                        (previous ++ separator ++ chunk) :: rest

                    [] ->
                        -- a false start before any real element: drop it
                        acc
        )
        []
        chunks
        |> List.reverse


endsName : String -> Bool
endsName chunk =
    case String.uncons chunk of
        Just ( c, _ ) ->
            isNameEnd c

        Nothing ->
            False


{-| The content of the first element `name`, between its start and end
tags. `Nothing` if there is none; `Just ""` if it is empty or self-closing.

    section "" "cellXfs" "<a><cellXfs count=\"1\"><xf/></cellXfs></a>" == Just "<xf/>"

-}
section : String -> String -> String -> Maybe String
section prefix name xml =
    case elements prefix name xml of
        first :: rest ->
            let
                whole =
                    String.join ("<" ++ prefix ++ name) (first :: rest)
            in
            case String.indexes ">" whole of
                i :: _ ->
                    if String.slice (i - 1) i whole == "/" then
                        Just ""

                    else
                        let
                            inner =
                                String.dropLeft (i + 1) whole
                        in
                        case String.indexes ("</" ++ prefix ++ name ++ ">") inner of
                            end :: _ ->
                                Just (String.left end inner)

                            [] ->
                                Just inner

                [] ->
                    Nothing

        [] ->
            Nothing


{-| The attributes part of an element chunk, up to (not including) the `>`
or `/>` that ends the start tag.
-}
startTag : String -> String
startTag chunk =
    case String.indexes ">" chunk of
        i :: _ ->
            if String.slice (i - 1) i chunk == "/" then
                String.left (i - 1) chunk

            else
                String.left i chunk

        [] ->
            chunk


{-| An attribute from a start tag, unescaped. Single and double quotes both
work.

    attribute "r" " r=\"A1\" t=\"s\"" == Just "A1"

-}
attribute : String -> String -> Maybe String
attribute name tag =
    case findAttribute (" " ++ name ++ "=") tag of
        Just value ->
            Just value

        Nothing ->
            -- attributes may also be separated by tabs or newlines
            if String.contains name tag then
                findAttribute (" " ++ name ++ "=") (String.map whitespaceToSpace tag)

            else
                Nothing


{-| A namespaced attribute, whatever its prefix: `prefixedAttribute "id"`
finds `r:id="rId1"` and `ns0:id="rId1"`.
-}
prefixedAttribute : String -> String -> Maybe String
prefixedAttribute name tag =
    findAttribute (":" ++ name ++ "=") (String.map whitespaceToSpace tag)


whitespaceToSpace : Char -> Char
whitespaceToSpace c =
    if c == '\t' || c == '\n' || c == '\u{000D}' then
        ' '

    else
        c


findAttribute : String -> String -> Maybe String
findAttribute needle tag =
    case String.indexes needle tag of
        i :: _ ->
            let
                rest =
                    String.dropLeft (i + String.length needle) tag
            in
            case String.uncons rest of
                Just ( quote, value ) ->
                    if quote == '"' || quote == '\'' then
                        case String.indexes (String.fromChar quote) value of
                            end :: _ ->
                                Just (Escape.unescape (String.left end value))

                            [] ->
                                Nothing

                    else
                        Nothing

                Nothing ->
                    Nothing

        [] ->
            Nothing


{-| The raw content of the first child element `name` inside a chunk, without
decoding. `Nothing` if there is no such element; `Just ""` for `<v/>`.

    content "" "v" " r=\"A1\"><v>42</v></c>" == Just "42"

-}
content : String -> String -> String -> Maybe String
content prefix name chunk =
    case elements prefix name chunk of
        first :: _ ->
            case String.indexes ">" first of
                i :: _ ->
                    if String.slice (i - 1) i first == "/" then
                        Just ""

                    else
                        let
                            inner =
                                String.dropLeft (i + 1) first
                        in
                        case String.indexes ("</" ++ prefix ++ name ++ ">") inner of
                            end :: _ ->
                                Just (String.left end inner)

                            [] ->
                                Just inner

                [] ->
                    Nothing

        [] ->
            Nothing


{-| The text of a string item (`<si>` or `<is>`): all its `<t>` elements,
concatenated and decoded, leaving out phonetic runs (`<rPh>`).
-}
textRuns : String -> String -> String
textRuns prefix chunk =
    withoutPhonetic prefix chunk
        |> elements prefix "t"
        |> List.map
            (\t ->
                case String.indexes ">" t of
                    i :: _ ->
                        if String.slice (i - 1) i t == "/" then
                            ""

                        else
                            let
                                inner =
                                    String.dropLeft (i + 1) t
                            in
                            case String.indexes ("</" ++ prefix ++ "t>") inner of
                                end :: _ ->
                                    String.left end inner

                                [] ->
                                    inner

                    [] ->
                        ""
            )
        |> String.concat
        |> decodeText


withoutPhonetic : String -> String -> String
withoutPhonetic prefix chunk =
    if String.contains ("<" ++ prefix ++ "rPh") chunk then
        case String.split ("<" ++ prefix ++ "rPh") chunk of
            first :: rest ->
                String.concat
                    (first
                        :: List.map
                            (\part ->
                                case String.indexes ("</" ++ prefix ++ "rPh>") part of
                                    i :: _ ->
                                        String.dropLeft (i + String.length ("</" ++ prefix ++ "rPh>")) part

                                    [] ->
                                        ""
                            )
                            rest
                    )

            [] ->
                chunk

    else
        chunk


{-| Decode raw element text: line endings (`\r\n` → `\n`, as XML parsers
do, before entities so `&#13;` survives), CDATA sections, XML entities and
OOXML's `_xHHHH_` escapes.
-}
decodeText : String -> String
decodeText raw =
    let
        normalized =
            normalizeNewlines raw

        decoded =
            if String.contains "<![CDATA[" normalized then
                decodeCdata normalized

            else
                Escape.unescape normalized
    in
    Escape.decodeXString decoded


normalizeNewlines : String -> String
normalizeNewlines s =
    if String.contains "\u{000D}" s then
        s
            |> String.replace "\u{000D}\n" "\n"
            |> String.replace "\u{000D}" "\n"

    else
        s


decodeCdata : String -> String
decodeCdata raw =
    case String.split "<![CDATA[" raw of
        first :: rest ->
            String.concat
                (Escape.unescape first
                    :: List.map
                        (\part ->
                            case String.indexes "]]>" part of
                                i :: _ ->
                                    String.left i part ++ Escape.unescape (String.dropLeft (i + 3) part)

                                [] ->
                                    part
                        )
                        rest
                )

        [] ->
            raw
