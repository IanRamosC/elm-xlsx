module FixturesTest exposing (suite)

{-| Files from other writers (openpyxl, XlsxWriter, and hand-built files in
the style of the .NET Open XML SDK and ISO 29500 Strict). See
`scripts/make_edge_fixtures.py`.
-}

import Base64
import Date
import Expect
import Fixtures
import Test exposing (Test, describe, test)
import Time exposing (Month(..))
import Xlsx.Date as XDate
import Xlsx.Read as Read exposing (CellValue(..))
import Xlsx.Ref as Ref


open : String -> Result String Read.Workbook
open b64 =
    case Base64.toBytes b64 of
        Just bytes ->
            Read.read bytes |> Result.mapError Read.errorToString

        Nothing ->
            Err "bad base64"


rowsOf : String -> String -> Result String (List (List CellValue))
rowsOf name b64 =
    open b64 |> Result.andThen (Read.sheet name >> Result.mapError Read.errorToString) |> Result.map Read.rows


at : String -> Read.Sheet -> Read.Cell
at ref sheet =
    case Ref.fromString ref of
        Just r ->
            Read.cell r sheet

        Nothing ->
            { value = ErrorValue ("bad ref " ++ ref), formula = Nothing, numberFormat = "", hyperlink = Nothing }


suite : Test
suite =
    describe "fixtures from other writers"
        [ test "every fixture opens and every worksheet parses" <|
            \_ ->
                [ Fixtures.openpyxl1904, Fixtures.openpyxlTypes, Fixtures.sdkStyle, Fixtures.strict, Fixtures.autofilter01, Fixtures.format01, Fixtures.mergeRange01, Fixtures.panes01, Fixtures.setColumn01, Fixtures.setRow01, Fixtures.simple01, Fixtures.hyperlink03, Fixtures.hyperlink31, Fixtures.properties01 ]
                    |> List.map
                        (\b64 ->
                            open b64
                                |> Result.andThen
                                    (\wb ->
                                        Read.sheetNames wb
                                            |> List.map (\n -> Read.sheet n wb |> Result.mapError Read.errorToString)
                                            |> List.filterMap
                                                (\r ->
                                                    case r of
                                                        Err e ->
                                                            Just e

                                                        Ok _ ->
                                                            Nothing
                                                )
                                            |> (\errors ->
                                                    if List.isEmpty errors then
                                                        Ok ()

                                                    else
                                                        Err (String.join "; " errors)
                                               )
                                    )
                        )
                    |> List.filter ((/=) (Ok ()))
                    |> Expect.equal []
        , test "hyperlinks in Excel-made files" <|
            \_ ->
                open Fixtures.hyperlink03
                    |> Result.andThen (Read.sheet "Sheet2" >> Result.mapError Read.errorToString)
                    |> Result.map (\s -> List.map (\r -> (at r s).hyperlink) [ "C2", "C5", "C7", "A1" ])
                    |> Expect.equal (Ok [ Just "http://www.google.com/", Just "http://www.cpan.org/", Just "http://www.perl.org/", Nothing ])
        , test "XlsxWriter simple01" <|
            \_ -> rowsOf "Sheet1" Fixtures.simple01 |> Expect.equal (Ok [ [ Text "Hello" ], [ Number 123 ] ])
        , describe "SDK style: prefixes, inline strings, missing r, shared formulas"
            [ test "values" <|
                \_ ->
                    rowsOf "Data" Fixtures.sdkStyle
                        |> Result.map (List.take 2)
                        |> Expect.equal
                            (Ok
                                [ [ Text "shared", Text "rich text", Text "東京", Text "a\nb", Text "\u{0001} and _x0041_" ]
                                , [ Text "inline", DateValue (Date.fromCalendarDate 2024 Feb 29), DateTimeValue (XDate.dateTime (Date.fromCalendarDate 2024 Feb 29) 12 0 0) ]
                                ]
                            )
            , test "formulas" <|
                \_ ->
                    open Fixtures.sdkStyle
                        |> Result.andThen (Read.sheet "Data" >> Result.mapError Read.errorToString)
                        |> Result.map (\s -> List.map (\r -> (at r s).formula) [ "B5", "B6", "C7" ])
                        |> Expect.equal (Ok [ Just "A5*10", Nothing, Just "CONCAT(\"a\",\"b\")" ])
            , test "chart sheet listed, not readable" <|
                \_ ->
                    open Fixtures.sdkStyle
                        |> Result.map (\wb -> ( List.map .kind (Read.sheets wb), Read.sheetNames wb ))
                        |> Expect.equal (Ok ( [ Read.Worksheet, Read.Chartsheet ], [ "Data" ] ))
            ]
        , test "Strict namespaces and a non-standard main part" <|
            \_ -> rowsOf "Strict" Fixtures.strict |> Expect.equal (Ok [ [ Text "strict", Number 1.5, Boolean False ] ])
        , describe "openpyxl"
            [ test "visibility" <|
                \_ ->
                    open Fixtures.openpyxlTypes
                        |> Result.map (Read.sheets >> List.map (\s -> ( s.name, s.visibility )))
                        |> Expect.equal (Ok [ ( "Types", Read.Visible ), ( "Hidden", Read.Hidden ), ( "Very", Read.VeryHidden ) ])
            , test "types, durations, times, rich text, merges" <|
                \_ ->
                    open Fixtures.openpyxlTypes
                        |> Result.andThen (Read.sheet "Types" >> Result.mapError Read.errorToString)
                        |> Result.map
                            (\s ->
                                ( List.map (\r -> (at r s).value) [ "A2", "C2", "D2", "E2", "G2", "H2", "A3", "A4" ]
                                , ( (at "F2" s).numberFormat, (at "I2" s).formula )
                                , List.map Ref.rangeToString (Read.merges s)
                                )
                            )
                        |> Expect.equal
                            (Ok
                                ( [ Text "héllo 🌈"
                                  , DateValue (Date.fromCalendarDate 2024 Feb 29)
                                  , DateTimeValue (XDate.dateTime (Date.fromCalendarDate 2024 Feb 29) 13 45 30)
                                  , DateTimeValue (XDate.dateTime (Date.fromCalendarDate 1899 Dec 31) 6 30 0)
                                  , Number 0.125
                                  , Boolean True
                                  , Text "line one\nline two"
                                  , Text "bold and plain"
                                  ]
                                , ( "[h]:mm:ss", Just "B2*2" )
                                , [ "A6:C7" ]
                                )
                            )
            , test "1904 date system" <|
                \_ ->
                    rowsOf "Sheet" Fixtures.openpyxl1904
                        |> Expect.equal
                            (Ok
                                [ [ DateValue (Date.fromCalendarDate 1904 Jan 1) ]
                                , [ DateValue (Date.fromCalendarDate 2024 Feb 29) ]
                                , [ DateTimeValue (XDate.dateTime (Date.fromCalendarDate 2000 Jan 1) 12 0 0) ]
                                ]
                            )
            ]
        ]
