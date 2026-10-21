module Xlsx.Internal.Styles exposing
    ( Registry, empty, register
    , toXml
    )

{-| Collect the distinct cell formats used in a workbook and write
`xl/styles.xml`.

Each distinct `Style` becomes one entry of `cellXfs`; a cell's `s` attribute is
the index of that entry. Index 0 is always the default format.

@docs Registry, empty, register
@docs toXml

-}

import Dict exposing (Dict)
import Xlsx.Internal.Model exposing (Style)
import Xlsx.Internal.NumFmt as NumFmt
import Xlsx.Internal.Xml.Escape as Escape


{-| -}
type Registry
    = Registry
        { customFormats : Dict String Int
        , customFormatsReversed : List ( Int, String )
        , xfIndexes : Dict String Int
        , xfsReversed : List Xf
        , xfCount : Int
        }


type alias Xf =
    { numFmtId : Int
    }


{-| A registry holding only the default format.
-}
empty : Registry
empty =
    Registry
        { customFormats = Dict.empty
        , customFormatsReversed = []
        , xfIndexes = Dict.singleton (xfKey defaultXf) 0
        , xfsReversed = [ defaultXf ]
        , xfCount = 1
        }


defaultXf : Xf
defaultXf =
    { numFmtId = 0 }


{-| The `cellXfs` index for a style, adding it if it is new.
-}
register : Style -> Registry -> ( Int, Registry )
register style registry =
    let
        ( numFmtId, withFormat ) =
            case style.numberFormat of
                Nothing ->
                    ( 0, registry )

                Just code ->
                    registerFormat code registry
    in
    registerXf { numFmtId = numFmtId } withFormat


registerFormat : String -> Registry -> ( Int, Registry )
registerFormat code ((Registry r) as registry) =
    case NumFmt.builtinId code of
        Just id ->
            ( id, registry )

        Nothing ->
            case Dict.get code r.customFormats of
                Just id ->
                    ( id, registry )

                Nothing ->
                    let
                        id =
                            NumFmt.firstCustomId + Dict.size r.customFormats
                    in
                    ( id
                    , Registry
                        { r
                            | customFormats = Dict.insert code id r.customFormats
                            , customFormatsReversed = ( id, code ) :: r.customFormatsReversed
                        }
                    )


registerXf : Xf -> Registry -> ( Int, Registry )
registerXf xf ((Registry r) as registry) =
    let
        key =
            xfKey xf
    in
    case Dict.get key r.xfIndexes of
        Just index ->
            ( index, registry )

        Nothing ->
            ( r.xfCount
            , Registry
                { r
                    | xfIndexes = Dict.insert key r.xfCount r.xfIndexes
                    , xfsReversed = xf :: r.xfsReversed
                    , xfCount = r.xfCount + 1
                }
            )


xfKey : Xf -> String
xfKey xf =
    String.fromInt xf.numFmtId


{-| The `styleSheet` part, its elements in schema order.
-}
toXml : Registry -> String
toXml (Registry r) =
    String.concat
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
        , "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
        , numFmtsXml (List.reverse r.customFormatsReversed)
        , "<fonts count=\"1\"><font><sz val=\"11\"/><color rgb=\"FF000000\"/><name val=\"Calibri\"/><family val=\"2\"/></font></fonts>"
        , "<fills count=\"2\"><fill><patternFill patternType=\"none\"/></fill><fill><patternFill patternType=\"gray125\"/></fill></fills>"
        , "<borders count=\"1\"><border><left/><right/><top/><bottom/><diagonal/></border></borders>"
        , "<cellStyleXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/></cellStyleXfs>"
        , "<cellXfs count=\"" ++ String.fromInt r.xfCount ++ "\">"
        , String.concat (List.map xfXml (List.reverse r.xfsReversed))
        , "</cellXfs>"
        , "<cellStyles count=\"1\"><cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/></cellStyles>"
        , "<dxfs count=\"0\"/>"
        , "<tableStyles count=\"0\" defaultTableStyle=\"TableStyleMedium9\" defaultPivotStyle=\"PivotStyleLight16\"/>"
        , "</styleSheet>"
        ]


numFmtsXml : List ( Int, String ) -> String
numFmtsXml formats =
    if List.isEmpty formats then
        ""

    else
        "<numFmts count=\""
            ++ String.fromInt (List.length formats)
            ++ "\">"
            ++ String.concat
                (List.map
                    (\( id, code ) -> "<numFmt numFmtId=\"" ++ String.fromInt id ++ "\" formatCode=\"" ++ Escape.attribute code ++ "\"/>")
                    formats
                )
            ++ "</numFmts>"


xfXml : Xf -> String
xfXml xf =
    "<xf numFmtId=\""
        ++ String.fromInt xf.numFmtId
        ++ "\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\""
        ++ (if xf.numFmtId /= 0 then
                " applyNumberFormat=\"1\""

            else
                ""
           )
        ++ "/>"
