port module Main exposing (main)

{-| M0 performance spike. Driven from `run.mjs`, which times each command.

Every command stores its result in the model so later commands can use it,
and reports a small summary so the work cannot be optimized away.

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Flate
import Json.Encode as E
import LZ77
import Parser exposing ((|.), (|=), Parser)
import Time
import Zip
import Zip.Entry


port run : (String -> msg) -> Sub msg


port done : E.Value -> Cmd msg


type alias Model =
    { xml : String
    , xmlBytes : Bytes
    , compressed : Bytes
    }


main : Program () Model String
main =
    Platform.worker
        { init = \_ -> ( { xml = "", xmlBytes = emptyBytes, compressed = emptyBytes }, Cmd.none )
        , update = update
        , subscriptions = \_ -> run identity
        }


emptyBytes : Bytes
emptyBytes =
    Bytes.Encode.encode (Bytes.Encode.sequence [])


update : String -> Model -> ( Model, Cmd String )
update cmd model =
    case String.split ":" cmd of
        [ "gen", n ] ->
            let
                xml =
                    sheetXml (Maybe.withDefault 0 (String.toInt n))
            in
            ( { model | xml = xml }, report [ ( "chars", E.int (String.length xml) ) ] )

        [ "encode" ] ->
            let
                bytes =
                    Bytes.Encode.encode (Bytes.Encode.string model.xml)
            in
            ( { model | xmlBytes = bytes }, report [ ( "bytes", E.int (Bytes.width bytes) ) ] )

        [ "deflate", "dynamic" ] ->
            deflateWith (Flate.Dynamic (Flate.WithWindowSize LZ77.maxWindowSize)) model

        [ "deflate", "static" ] ->
            deflateWith (Flate.Static (Flate.WithWindowSize LZ77.maxWindowSize)) model

        [ "deflate", "dynamic", w ] ->
            deflateWith (Flate.Dynamic (Flate.WithWindowSize (Maybe.withDefault 0 (String.toInt w)))) model

        [ "deflate", "raw" ] ->
            deflateWith Flate.Raw model

        [ "inflate" ] ->
            case Flate.inflate model.compressed of
                Just out ->
                    ( model, report [ ( "bytes", E.int (Bytes.width out) ), ( "same", E.bool (Bytes.width out == Bytes.width model.xmlBytes) ) ] )

                Nothing ->
                    ( model, report [ ( "error", E.string "inflate failed" ) ] )

        [ "decode" ] ->
            let
                str =
                    Bytes.Decode.decode (Bytes.Decode.string (Bytes.width model.xmlBytes)) model.xmlBytes
                        |> Maybe.withDefault ""
            in
            ( model, report [ ( "chars", E.int (String.length str) ) ] )

        [ "scan", "parser" ] ->
            ( model, reportScan (scanWithParser model.xml) )

        [ "scan", "split" ] ->
            ( model, reportScan (scanWithSplit model.xml) )

        [ "onecell" ] ->
            let
                bytes =
                    oneCellXlsx
            in
            ( model, report [ ( "file", E.list E.int (bytesToList bytes) ) ] )

        _ ->
            ( model, report [ ( "error", E.string ("unknown command " ++ cmd) ) ] )


deflateWith : Flate.Encoding -> Model -> ( Model, Cmd msg )
deflateWith encoding model =
    let
        out =
            Flate.deflateWithOptions encoding model.xmlBytes
    in
    ( { model | compressed = out }, report [ ( "bytes", E.int (Bytes.width out) ) ] )


report : List ( String, E.Value ) -> Cmd msg
report fields =
    done (E.object fields)


reportScan : Result String ScanResult -> Cmd msg
reportScan result =
    case result of
        Ok r ->
            report [ ( "cells", E.int r.cells ), ( "sum", E.float r.sum ) ]

        Err e ->
            report [ ( "error", E.string e ) ]



-- WRITING


columns : Int
columns =
    10


{-| Build a worksheet with `n` cells, ten per row: alternating shared-string
indexes and numbers, as a realistic export would look.
-}
sheetXml : Int -> String
sheetXml n =
    let
        rowCount =
            (n + columns - 1) // columns

        cell r c acc =
            let
                ref =
                    String.fromChar (Char.fromCode (64 + c)) ++ String.fromInt r
            in
            if modBy 2 c == 0 then
                "</v></c>" :: String.fromInt (r * c) :: "\" t=\"s\"><v>" :: ref :: "<c r=\"" :: acc

            else
                "</v></c>" :: String.fromFloat (toFloat r * 1.25 + toFloat c) :: "\"><v>" :: ref :: "<c r=\"" :: acc

        row r acc =
            "</row>"
                :: List.foldl (\c a -> cell r c a)
                    ("\">" :: String.fromInt r :: "<row r=\"" :: acc)
                    (List.range 1 columns)

        body =
            List.foldl row [ "<sheetData>", header ] (List.range 1 rowCount)
    in
    ("</worksheet>" :: "</sheetData>" :: body)
        |> List.reverse
        |> String.concat


header : String
header =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"



-- SCANNING


type alias ScanResult =
    { cells : Int, sum : Float }


{-| Prototype 1: elm/parser over the whole document.
-}
scanWithParser : String -> Result String ScanResult
scanWithParser xml =
    Parser.run (Parser.loop { cells = 0, sum = 0 } scanStep) xml
        |> Result.mapError (\_ -> "parse error")


scanStep : ScanResult -> Parser (Parser.Step ScanResult ScanResult)
scanStep acc =
    Parser.oneOf
        [ Parser.succeed (\v -> Parser.Loop { cells = acc.cells + 1, sum = acc.sum + Maybe.withDefault 0 (String.toFloat v) })
            |. Parser.chompUntil "<c "
            |. Parser.token "<c "
            |. Parser.chompUntil "<v>"
            |. Parser.token "<v>"
            |= Parser.getChompedString (Parser.chompUntil "</v>")
            |. Parser.token "</v>"
        , Parser.succeed (Parser.Done acc)
            |. Parser.chompUntilEndOr "\u{0000}"
        ]


{-| Prototype 2: native String.split, then small slices per cell.
-}
scanWithSplit : String -> Result String ScanResult
scanWithSplit xml =
    case String.split "<c " xml of
        [] ->
            Ok { cells = 0, sum = 0 }

        _ :: cells ->
            Ok (List.foldl scanCell { cells = 0, sum = 0 } cells)


scanCell : String -> ScanResult -> ScanResult
scanCell chunk acc =
    case String.indexes "<v>" chunk of
        start :: _ ->
            case String.indexes "</v>" chunk of
                end :: _ ->
                    { cells = acc.cells + 1
                    , sum = acc.sum + Maybe.withDefault 0 (String.toFloat (String.slice (start + 3) end chunk))
                    }

                [] ->
                    acc

        [] ->
            { acc | cells = acc.cells + 1 }



-- ONE-CELL FILE


{-| The smallest workbook we expect Excel to open without complaint:
a single sheet with "Hello" in A1 and 42 in B1.
-}
oneCellXlsx : Bytes
oneCellXlsx =
    let
        time =
            ( Time.utc, Time.millisToPosix 1794614400000 )

        entry ( path, content ) =
            Zip.Entry.compress { path = path, lastModified = time, comment = Nothing }
                (Bytes.Encode.encode (Bytes.Encode.string content))
    in
    [ ( "[Content_Types].xml"
      , xmlDecl
            ++ "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
            ++ "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
            ++ "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
            ++ "<Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>"
            ++ "<Override PartName=\"/xl/worksheets/sheet1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"
            ++ "<Override PartName=\"/xl/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml\"/>"
            ++ "</Types>"
      )
    , ( "_rels/.rels"
      , xmlDecl
            ++ "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
            ++ "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/>"
            ++ "</Relationships>"
      )
    , ( "xl/workbook.xml"
      , xmlDecl
            ++ "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">"
            ++ "<sheets><sheet name=\"Sheet1\" sheetId=\"1\" r:id=\"rId1\"/></sheets>"
            ++ "</workbook>"
      )
    , ( "xl/_rels/workbook.xml.rels"
      , xmlDecl
            ++ "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
            ++ "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet1.xml\"/>"
            ++ "<Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>"
            ++ "</Relationships>"
      )
    , ( "xl/styles.xml"
      , xmlDecl
            ++ "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
            ++ "<fonts count=\"1\"><font><sz val=\"11\"/><name val=\"Calibri\"/><family val=\"2\"/></font></fonts>"
            ++ "<fills count=\"2\"><fill><patternFill patternType=\"none\"/></fill><fill><patternFill patternType=\"gray125\"/></fill></fills>"
            ++ "<borders count=\"1\"><border><left/><right/><top/><bottom/><diagonal/></border></borders>"
            ++ "<cellStyleXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/></cellStyleXfs>"
            ++ "<cellXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\"/></cellXfs>"
            ++ "<cellStyles count=\"1\"><cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/></cellStyles>"
            ++ "</styleSheet>"
      )
    , ( "xl/worksheets/sheet1.xml"
      , xmlDecl
            ++ "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
            ++ "<dimension ref=\"A1:B1\"/>"
            ++ "<sheetData><row r=\"1\"><c r=\"A1\" t=\"inlineStr\"><is><t>Hello</t></is></c><c r=\"B1\"><v>42</v></c></row></sheetData>"
            ++ "<pageMargins left=\"0.7\" right=\"0.7\" top=\"0.75\" bottom=\"0.75\" header=\"0.3\" footer=\"0.3\"/>"
            ++ "</worksheet>"
      )
    ]
        |> List.map entry
        |> Zip.fromEntries
        |> Zip.toBytes


xmlDecl : String
xmlDecl =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"


bytesToList : Bytes -> List Int
bytesToList bytes =
    let
        step ( n, acc ) =
            if n <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse acc))

            else
                Bytes.Decode.map (\b -> Bytes.Decode.Loop ( n - 1, b :: acc )) Bytes.Decode.unsignedInt8
    in
    Bytes.Decode.decode (Bytes.Decode.loop ( Bytes.width bytes, [] ) step) bytes
        |> Maybe.withDefault []
