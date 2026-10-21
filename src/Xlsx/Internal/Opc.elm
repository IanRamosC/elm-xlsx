module Xlsx.Internal.Opc exposing
    ( xmlDeclaration
    , ContentTypes, contentTypes
    , Relationship, relationships
    , relType, contentType
    )

{-| Open Packaging Conventions: the `[Content_Types].xml` part and `.rels`
relationship parts that tie the package together.

@docs xmlDeclaration
@docs ContentTypes, contentTypes
@docs Relationship, relationships
@docs relType, contentType

-}

import Xlsx.Internal.Xml.Escape as Escape


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
