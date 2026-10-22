module Xlsx.Internal.DocProps exposing (core, app, iso8601)

{-| Document properties: `docProps/core.xml` (author, dates) and
`docProps/app.xml` (application, sheet names).

@docs core, app, iso8601

-}

import Time
import Xlsx.Internal.Opc as Opc
import Xlsx.Internal.Xml.Escape as Escape


{-| The core properties part.
-}
core : { created : Time.Posix } -> String
core { created } =
    let
        stamp =
            Escape.text (iso8601 created)
    in
    String.concat
        [ Opc.xmlDeclaration
        , "<cp:coreProperties xmlns:cp=\"http://schemas.openxmlformats.org/package/2006/metadata/core-properties\""
        , " xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\""
        , " xmlns:dcmitype=\"http://purl.org/dc/dcmitype/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">"
        , "<dc:creator></dc:creator><cp:lastModifiedBy></cp:lastModifiedBy>"
        , "<dcterms:created xsi:type=\"dcterms:W3CDTF\">" ++ stamp ++ "</dcterms:created>"
        , "<dcterms:modified xsi:type=\"dcterms:W3CDTF\">" ++ stamp ++ "</dcterms:modified>"
        , "</cp:coreProperties>"
        ]


{-| The extended (application) properties part.

`Application` says "Microsoft Excel", as XlsxWriter and most other writers do,
because some readers check it.

-}
app : { sheetNames : List String } -> String
app { sheetNames } =
    let
        count =
            String.fromInt (List.length sheetNames)
    in
    String.concat
        [ Opc.xmlDeclaration
        , "<Properties xmlns=\"http://schemas.openxmlformats.org/officeDocument/2006/extended-properties\""
        , " xmlns:vt=\"http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes\">"
        , "<Application>Microsoft Excel</Application><DocSecurity>0</DocSecurity><ScaleCrop>false</ScaleCrop>"
        , "<HeadingPairs><vt:vector size=\"2\" baseType=\"variant\">"
        , "<vt:variant><vt:lpstr>Worksheets</vt:lpstr></vt:variant>"
        , "<vt:variant><vt:i4>" ++ count ++ "</vt:i4></vt:variant>"
        , "</vt:vector></HeadingPairs>"
        , "<TitlesOfParts><vt:vector size=\"" ++ count ++ "\" baseType=\"lpstr\">"
        , String.concat (List.map (\name -> "<vt:lpstr>" ++ Escape.text name ++ "</vt:lpstr>") sheetNames)
        , "</vt:vector></TitlesOfParts>"
        , "<Company></Company><LinksUpToDate>false</LinksUpToDate><SharedDoc>false</SharedDoc>"
        , "<HyperlinksChanged>false</HyperlinksChanged><AppVersion>12.0000</AppVersion>"
        , "</Properties>"
        ]


{-| A UTC timestamp such as `2026-11-13T09:30:00Z`.
-}
iso8601 : Time.Posix -> String
iso8601 t =
    let
        pad n =
            String.padLeft 2 '0' (String.fromInt n)
    in
    String.fromInt (Time.toYear Time.utc t)
        ++ "-"
        ++ pad (monthNumber (Time.toMonth Time.utc t))
        ++ "-"
        ++ pad (Time.toDay Time.utc t)
        ++ "T"
        ++ pad (Time.toHour Time.utc t)
        ++ ":"
        ++ pad (Time.toMinute Time.utc t)
        ++ ":"
        ++ pad (Time.toSecond Time.utc t)
        ++ "Z"


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
