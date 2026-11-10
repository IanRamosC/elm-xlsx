module Xlsx.Internal.Opc exposing
    ( xmlDeclaration
    , ContentTypes, contentTypes
    , Relationship, relationships
    , relType, contentType
    , parseRelationships, relsPathFor, resolveTarget, isRelType
    )

{-| Open Packaging Conventions: the `[Content_Types].xml` part and `.rels`
relationship parts that tie the package together.

@docs xmlDeclaration
@docs ContentTypes, contentTypes
@docs Relationship, relationships
@docs relType, contentType


# Reading

@docs parseRelationships, relsPathFor, resolveTarget, isRelType

-}

import Url
import Xlsx.Internal.Xml.Escape as Escape
import Xlsx.Internal.Xml.Scan as Scan


{-| -}
xmlDeclaration : String
xmlDeclaration =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"


{-| Absolute part names (`/xl/workbook.xml`) and their content types.
-}
type alias ContentTypes =
    List ( String, String )


{-| The `[Content_Types].xml` part: defaults for `.rels` and `.xml`, plus an
override for every part.
-}
contentTypes : ContentTypes -> String
contentTypes overrides =
    String.concat
        (xmlDeclaration
            :: "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
            :: "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
            :: "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
            :: List.map
                (\( part, ct ) -> "<Override PartName=\"" ++ Escape.attribute part ++ "\" ContentType=\"" ++ ct ++ "\"/>")
                overrides
            ++ [ "</Types>" ]
        )


{-| One link from a part to another part or to an external URL.
-}
type alias Relationship =
    { id : String
    , type_ : String
    , target : String
    , external : Bool
    }


{-| A `.rels` part.
-}
relationships : List Relationship -> String
relationships rels =
    String.concat
        (xmlDeclaration
            :: "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
            :: List.map relationshipXml rels
            ++ [ "</Relationships>" ]
        )


relationshipXml : Relationship -> String
relationshipXml rel =
    "<Relationship Id=\""
        ++ rel.id
        ++ "\" Type=\""
        ++ rel.type_
        ++ "\" Target=\""
        ++ Escape.attribute rel.target
        ++ (if rel.external then
                "\" TargetMode=\"External\"/>"

            else
                "\"/>"
           )


{-| Relationship types (Transitional OOXML).
-}
relType :
    { officeDocument : String
    , coreProperties : String
    , extendedProperties : String
    , worksheet : String
    , styles : String
    , sharedStrings : String
    , hyperlink : String
    }
relType =
    { officeDocument = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
    , coreProperties = "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"
    , extendedProperties = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties"
    , worksheet = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet"
    , styles = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"
    , sharedStrings = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings"
    , hyperlink = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"
    }


{-| Content types of the parts we write.
-}
contentType :
    { workbook : String
    , worksheet : String
    , styles : String
    , sharedStrings : String
    , coreProperties : String
    , extendedProperties : String
    }
contentType =
    { workbook = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"
    , worksheet = "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"
    , styles = "application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"
    , sharedStrings = "application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"
    , coreProperties = "application/vnd.openxmlformats-package.core-properties+xml"
    , extendedProperties = "application/vnd.openxmlformats-officedocument.extended-properties+xml"
    }



-- READING


{-| The relationships in a `.rels` part.
-}
parseRelationships : String -> Result String (List Relationship)
parseRelationships xml =
    Scan.document "Relationships" xml
        |> Result.map
            (\prefix ->
                Scan.elements prefix "Relationship" xml
                    |> List.filterMap
                        (\chunk ->
                            let
                                tag =
                                    Scan.startTag chunk
                            in
                            Maybe.map3
                                (\id type_ target ->
                                    { id = id
                                    , type_ = type_
                                    , target = target
                                    , external = Scan.attribute "TargetMode" tag == Just "External"
                                    }
                                )
                                (Scan.attribute "Id" tag)
                                (Scan.attribute "Type" tag)
                                (Scan.attribute "Target" tag)
                        )
            )


{-| The `.rels` part that holds a part's relationships:
`xl/workbook.xml` → `xl/_rels/workbook.xml.rels`, and the package itself
(`""`) → `_rels/.rels`.
-}
relsPathFor : String -> String
relsPathFor part =
    let
        ( dir, file ) =
            splitPath part
    in
    joinPath [ dir, "_rels", file ++ ".rels" ]


{-| Resolve a relationship target against the part it comes from. Targets are
relative to the source part's folder unless they start with `/`; `..` and
percent-encoding are handled. The result has no leading slash.

    resolveTarget "xl/workbook.xml" "worksheets/sheet1.xml" == "xl/worksheets/sheet1.xml"

    resolveTarget "xl/workbook.xml" "/xl/styles.xml" == "xl/styles.xml"

    resolveTarget "xl/worksheets/sheet1.xml" "../drawings/drawing1.xml" == "xl/drawings/drawing1.xml"

-}
resolveTarget : String -> String -> String
resolveTarget source target =
    let
        decoded =
            Url.percentDecode target |> Maybe.withDefault target

        withoutFragment =
            case String.split "#" decoded of
                first :: _ ->
                    first

                [] ->
                    decoded

        segments =
            if String.startsWith "/" withoutFragment then
                String.split "/" withoutFragment

            else
                String.split "/" (Tuple.first (splitPath source)) ++ String.split "/" withoutFragment
    in
    segments
        |> List.foldl
            (\segment acc ->
                case segment of
                    "" ->
                        acc

                    "." ->
                        acc

                    ".." ->
                        List.drop 1 acc

                    _ ->
                        segment :: acc
            )
            []
        |> List.reverse
        |> String.join "/"


{-| Whether a relationship has a type, compared by its last path segment so
Transitional (`schemas.openxmlformats.org/...`) and Strict
(`purl.oclc.org/ooxml/...`) types both match.

    isRelType "worksheet" "http://purl.oclc.org/ooxml/officeDocument/relationships/worksheet" == True

-}
isRelType : String -> String -> Bool
isRelType kind type_ =
    String.endsWith ("/" ++ kind) type_


splitPath : String -> ( String, String )
splitPath path =
    case List.reverse (String.split "/" path) of
        file :: reversedDir ->
            ( String.join "/" (List.reverse reversedDir), file )

        [] ->
            ( "", path )


joinPath : List String -> String
joinPath parts =
    String.join "/" (List.filter (not << String.isEmpty) parts)
