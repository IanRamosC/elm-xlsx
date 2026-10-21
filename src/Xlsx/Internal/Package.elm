module Xlsx.Internal.Package exposing (Compression(..), write)

{-| Assemble every part of a workbook into the ZIP package.

@docs Compression, write

-}

import Bytes exposing (Bytes)
import Bytes.Encode
import Time
import Xlsx.Internal.DocProps as DocProps
import Xlsx.Internal.Limits as Limits
import Xlsx.Internal.Model exposing (Problem, Sheet(..), WorkbookData)
import Xlsx.Internal.Opc as Opc exposing (contentType, relType)
import Xlsx.Internal.SharedStrings as SharedStrings
import Xlsx.Internal.SheetWriter as SheetWriter
import Xlsx.Internal.Styles as Styles
import Xlsx.Internal.Xml.Escape as Escape
import Zip
import Zip.Entry


{-| `Auto` deflates parts below `threshold` bytes and stores bigger ones.
-}
type Compression
    = Auto Int
    | Deflate
    | Store


{-| Write the package, or report every problem found.
-}
write : { created : Time.Posix, compression : Compression } -> WorkbookData -> Result (List Problem) Bytes
write options workbook =
    let
        sheets =
            List.map (\(Sheet s) -> s) workbook.sheets

        names =
            List.map .name sheets

        activeIndex =
            workbook.activeSheet
                |> Maybe.andThen (\name -> indexOf name names)
                |> Maybe.withDefault 0

        workbookProblems =
            Limits.checkWorkbook { sheetNames = names, activeSheet = workbook.activeSheet }

        ( sheetXmls, state ) =
            List.foldl
                (\( index, sheet ) ( acc, st ) ->
                    let
                        ( xml, next ) =
                            SheetWriter.write { selected = index == activeIndex } sheet st
                    in
                    ( xml :: acc, next )
                )
                ( [], SheetWriter.initialState )
                (List.indexedMap Tuple.pair sheets)
                |> Tuple.mapFirst List.reverse

        problems =
            workbookProblems ++ List.reverse state.problems
    in
    if List.isEmpty problems then
        Ok (zip options (parts { names = names, activeIndex = activeIndex, sheetXmls = sheetXmls, state = state, created = options.created }))

    else
        Err problems


type alias PartsInput =
    { names : List String
    , activeIndex : Int
    , sheetXmls : List String
    , state : SheetWriter.State
    , created : Time.Posix
    }


{-| Every part, in the order they go into the ZIP file.
-}
parts : PartsInput -> List ( String, String )
parts input =
    let
        sheetCount =
            List.length input.sheetXmls

        sheetPath n =
            "xl/worksheets/sheet" ++ String.fromInt n ++ ".xml"

        hasStrings =
            not (SharedStrings.isEmpty input.state.strings)

        sheetNumbers =
            List.range 1 sheetCount

        overrides =
            [ ( "/docProps/app.xml", contentType.extendedProperties )
            , ( "/docProps/core.xml", contentType.coreProperties )
            , ( "/xl/workbook.xml", contentType.workbook )
            , ( "/xl/styles.xml", contentType.styles )
            ]
                ++ List.map (\n -> ( "/" ++ sheetPath n, contentType.worksheet )) sheetNumbers
                ++ (if hasStrings then
                        [ ( "/xl/sharedStrings.xml", contentType.sharedStrings ) ]

                    else
                        []
                   )

        rootRels =
            [ { id = "rId1", type_ = relType.officeDocument, target = "xl/workbook.xml", external = False }
            , { id = "rId2", type_ = relType.coreProperties, target = "docProps/core.xml", external = False }
            , { id = "rId3", type_ = relType.extendedProperties, target = "docProps/app.xml", external = False }
            ]

        workbookRels =
            List.map (\n -> { id = "rId" ++ String.fromInt n, type_ = relType.worksheet, target = "worksheets/sheet" ++ String.fromInt n ++ ".xml", external = False }) sheetNumbers
                ++ [ { id = "rId" ++ String.fromInt (sheetCount + 1), type_ = relType.styles, target = "styles.xml", external = False } ]
                ++ (if hasStrings then
                        [ { id = "rId" ++ String.fromInt (sheetCount + 2), type_ = relType.sharedStrings, target = "sharedStrings.xml", external = False } ]

                    else
                        []
                   )
    in
    [ ( "[Content_Types].xml", Opc.contentTypes overrides )
    , ( "_rels/.rels", Opc.relationships rootRels )
    , ( "docProps/app.xml", DocProps.app { sheetNames = input.names } )
    , ( "docProps/core.xml", DocProps.core { created = input.created } )
    , ( "xl/workbook.xml", workbookXml input )
    , ( "xl/_rels/workbook.xml.rels", Opc.relationships workbookRels )
    , ( "xl/styles.xml", Styles.toXml input.state.styles )
    ]
        ++ (if hasStrings then
                [ ( "xl/sharedStrings.xml", SharedStrings.toXml input.state.strings ) ]

            else
                []
           )
        ++ List.map2 (\n xml -> ( sheetPath n, xml )) sheetNumbers input.sheetXmls


workbookXml : PartsInput -> String
workbookXml input =
    String.concat
        [ Opc.xmlDeclaration
        , "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\""
        , " xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">"
        , "<workbookPr/>"
        , "<bookViews><workbookView xWindow=\"240\" yWindow=\"15\" windowWidth=\"16095\" windowHeight=\"9660\""
            ++ (if input.activeIndex > 0 then
                    " activeTab=\"" ++ String.fromInt input.activeIndex ++ "\""

                else
                    ""
               )
            ++ "/></bookViews>"
        , "<sheets>"
        , String.concat
            (List.indexedMap
                (\i name ->
                    "<sheet name=\""
                        ++ Escape.attribute name
                        ++ "\" sheetId=\""
                        ++ String.fromInt (i + 1)
                        ++ "\" r:id=\"rId"
                        ++ String.fromInt (i + 1)
                        ++ "\"/>"
                )
                input.names
            )
        , "</sheets>"
        , if input.state.needsRecalc then
            "<calcPr calcId=\"124519\" fullCalcOnLoad=\"1\"/>"

          else
            "<calcPr calcId=\"124519\"/>"
        , "</workbook>"
        ]


zip : { created : Time.Posix, compression : Compression } -> List ( String, String ) -> Bytes
zip options files =
    files
        |> List.map
            (\( path, content ) ->
                let
                    bytes =
                        Bytes.Encode.encode (Bytes.Encode.string content)

                    meta =
                        { path = path, lastModified = ( Time.utc, options.created ), comment = Nothing }
                in
                if shouldDeflate options.compression (Bytes.width bytes) then
                    Zip.Entry.compress meta bytes

                else
                    Zip.Entry.store meta bytes
            )
        |> Zip.fromEntries
        |> Zip.toBytes


shouldDeflate : Compression -> Int -> Bool
shouldDeflate compression size =
    case compression of
        Auto threshold ->
            size < threshold

        Deflate ->
            True

        Store ->
            False


indexOf : a -> List a -> Maybe Int
indexOf x list =
    indexOfHelp x list 0


indexOfHelp : a -> List a -> Int -> Maybe Int
indexOfHelp x list i =
    case list of
        [] ->
            Nothing

        y :: rest ->
            if x == y then
                Just i

            else
                indexOfHelp x rest (i + 1)
