module Xlsx.Internal.Xml.Tree exposing
    ( Element, parse
    , name, attribute, children, child, descendants, text
    , localName, hasDoctype
    )

{-| A small DOM for the XML parts we read whole (workbook, relationships,
styles). Built on `miniBill/elm-xml-parser`, with two OOXML-specific rules:

  - Element and attribute names are matched by **local name**, so `x:row`,
    `row` and `ns0:row` are the same. Files written by the .NET Open XML SDK
    use prefixes; most others do not.
  - Documents with a `<!DOCTYPE` are rejected before parsing. OOXML never
    uses one, and refusing them rules out entity-expansion attacks.

@docs Element, parse
@docs name, attribute, children, child, descendants, text
@docs localName, hasDoctype

-}

import XmlParser


{-| An element with its local name, attributes (local names too) and child
elements and text.
-}
type Element
    = Element
        { name : String
        , attributes : List ( String, String )
        , content : List Node
        }


type Node
    = ElementNode Element
    | TextNode String


{-| Parse a document and give back its root element.
-}
parse : String -> Result String Element
parse source =
    if hasDoctype source then
        Err "The XML has a DOCTYPE, which OOXML never uses."

    else
        case XmlParser.parse source of
            Ok xml ->
                case convert xml.root of
                    ElementNode root ->
                        Ok root

                    TextNode _ ->
                        Err "The XML has no root element."

            Err deadEnds ->
                Err
                    (case deadEnds of
                        first :: _ ->
                            "The XML is malformed near line " ++ String.fromInt first.row ++ ", column " ++ String.fromInt first.col ++ "."

                        [] ->
                            "The XML is malformed."
                    )


{-| XML requires `DOCTYPE` in capitals, but some parsers accept any case.
-}
hasDoctype : String -> Bool
hasDoctype source =
    String.contains "<!DOCTYPE" source || String.contains "<!doctype" source


convert : XmlParser.Node -> Node
convert node =
    case node of
        XmlParser.Element tag attrs content ->
            ElementNode
                (Element
                    { name = localName tag
                    , attributes = List.map (\a -> ( localName a.name, a.value )) attrs
                    , content = List.map convert content
                    }
                )

        XmlParser.Text s ->
            TextNode s


{-| The part of a name after its namespace prefix: `x:row` → `row`.
-}
localName : String -> String
localName qualified =
    case String.indexes ":" qualified of
        [] ->
            qualified

        indexes ->
            String.dropLeft (List.foldl max 0 indexes + 1) qualified


{-| -}
name : Element -> String
name (Element e) =
    e.name


{-| An attribute by local name. `r:id` is found as `"id"`.
-}
attribute : String -> Element -> Maybe String
attribute key (Element e) =
    findFirst (\( k, _ ) -> k == key) e.attributes |> Maybe.map Tuple.second


{-| Child elements with a local name.
-}
children : String -> Element -> List Element
children key (Element e) =
    List.filterMap
        (\node ->
            case node of
                ElementNode ((Element c) as el) ->
                    if c.name == key then
                        Just el

                    else
                        Nothing

                TextNode _ ->
                    Nothing
        )
        e.content


{-| The first child element with a local name.
-}
child : String -> Element -> Maybe Element
child key el =
    List.head (children key el)


{-| Every element below this one with a local name, in document order,
without looking inside matching elements.
-}
descendants : String -> Element -> List Element
descendants key (Element e) =
    List.concatMap
        (\node ->
            case node of
                ElementNode ((Element c) as el) ->
                    if c.name == key then
                        [ el ]

                    else
                        descendants key el

                TextNode _ ->
                    []
        )
        e.content


{-| All the text directly inside an element.
-}
text : Element -> String
text (Element e) =
    e.content
        |> List.filterMap
            (\node ->
                case node of
                    TextNode s ->
                        Just s

                    ElementNode _ ->
                        Nothing
            )
        |> String.concat


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst predicate list =
    case list of
        [] ->
            Nothing

        x :: rest ->
            if predicate x then
                Just x

            else
                findFirst predicate rest
