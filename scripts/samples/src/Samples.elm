port module Samples exposing (main)

{-| Sample workbooks for external validation. `run.mjs` writes each one to
`scripts/out/<name>.xlsx`, plus `<name>.json` listing the cells it contains,
and `scripts/validate.py` checks them with openpyxl and python-calamine.
-}

import Base64
import Date exposing (Date)
import Json.Encode as E
import Time exposing (Month(..))
import Xlsx.Date as XDate
import Xlsx.Ref as Ref
import Xlsx.Write as W


port start : (() -> msg) -> Sub msg


port emit : E.Value -> Cmd msg


main : Program () () ()
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \_ m -> ( m, Cmd.batch (List.map (emitSample >> emit) samples) )
        , subscriptions = \_ -> start identity
        }


{-| A cell we expect to read back: its kind and value as JSON.
-}
type Expected
    = S String
    | N Float
    | B Bool
    | D Date
    | DT XDate.DateTime
    | Err String
    | F String Expected
    | FNoValue String


type alias Sample =
    { name : String
    , sheets : List ( String, List (List ( W.Cell, Maybe Expected )) )
    , active : Maybe String
    , compression : W.Compression
    }


emitSample : Sample -> E.Value
emitSample sample =
    let
        wb =
            W.workbook
                (List.map
                    (\( name, rows ) -> W.sheet name (List.map (List.map Tuple.first >> W.row) rows))
                    sample.sheets
                )
                |> (case sample.active of
                        Just a ->
                            W.withActiveSheet a

                        Nothing ->
                            identity
                   )

        expected =
            sample.sheets
                |> List.concatMap
                    (\( name, rows ) ->
                        rows
                            |> List.indexedMap
                                (\r cells ->
                                    cells
                                        |> List.indexedMap
                                            (\c ( _, exp ) ->
                                                exp
                                                    |> Maybe.map
                                                        (\e ->
                                                            E.object
                                                                [ ( "sheet", E.string name )
                                                                , ( "ref", E.string (Ref.toString { column = c + 1, row = r + 1 }) )
                                                                , ( "expect", encodeExpected e )
                                                                ]
                                                        )
                                            )
                                        |> List.filterMap identity
                                )
                            |> List.concat
                    )
    in
    case W.toBytes { created = Time.millisToPosix 1794528000000, compression = sample.compression } wb of
        Ok bytes ->
            E.object
                [ ( "name", E.string sample.name )
                , ( "base64", E.string (Maybe.withDefault "" (Base64.fromBytes bytes)) )
                , ( "cells", E.list identity expected )
                , ( "sheets", E.list (Tuple.first >> E.string) sample.sheets )
                , ( "active", E.string (Maybe.withDefault (Maybe.withDefault "" (List.head (List.map Tuple.first sample.sheets))) sample.active) )
                ]

        Result.Err problems ->
            E.object
                [ ( "name", E.string sample.name )
                , ( "problems", E.list (W.problemToString >> E.string) problems )
                ]


encodeExpected : Expected -> E.Value
encodeExpected e =
    case e of
        S s ->
            E.object [ ( "type", E.string "string" ), ( "value", E.string s ) ]

        N n ->
            E.object [ ( "type", E.string "number" ), ( "value", E.float n ) ]

        B b ->
            E.object [ ( "type", E.string "bool" ), ( "value", E.bool b ) ]

        D d ->
            E.object [ ( "type", E.string "date" ), ( "value", E.string (Date.toIsoString d) ) ]

        DT dt ->
            E.object
                [ ( "type", E.string "datetime" )
                , ( "value"
                  , E.string
                        (Date.toIsoString dt.date
                            ++ "T"
                            ++ pad dt.hour
                            ++ ":"
                            ++ pad dt.minute
                            ++ ":"
                            ++ pad dt.second
                            ++ "."
                            ++ String.padLeft 3 '0' (String.fromInt dt.millisecond)
                        )
                  )
                ]

        Err code ->
            E.object [ ( "type", E.string "error" ), ( "value", E.string code ) ]

        F f cached ->
            E.object [ ( "type", E.string "formula" ), ( "formula", E.string f ), ( "cached", encodeExpected cached ) ]

        FNoValue f ->
            E.object [ ( "type", E.string "formula" ), ( "formula", E.string f ), ( "cached", E.null ) ]


pad : Int -> String
pad n =
    String.padLeft 2 '0' (String.fromInt n)



-- SAMPLES


samples : List Sample
samples =
    [ simple, types, text, multiSheet, large ]


makeSample : String -> List ( String, List (List ( W.Cell, Maybe Expected )) ) -> Sample
makeSample name sheets =
    { name = name, sheets = sheets, active = Nothing, compression = W.Auto }


{-| Same content as XlsxWriter's `simple01` comparison file.
-}
simple : Sample
simple =
    makeSample "simple"
        [ ( "Sheet1"
          , [ [ ( W.string "Hello", Just (S "Hello") ) ]
            , [ ( W.int 123, Just (N 123) ) ]
            ]
          )
        ]


types : Sample
types =
    let
        d =
            Date.fromCalendarDate 2026 Nov 13

        dt =
            XDate.dateTime (Date.fromCalendarDate 2000 Feb 29) 13 45 30
    in
    makeSample "types"
        [ ( "Types"
          , [ [ ( W.string "text", Just (S "text") ), ( W.float 3.14159, Just (N 3.14159) ), ( W.int -42, Just (N -42) ) ]
            , [ ( W.bool True, Just (B True) ), ( W.bool False, Just (B False) ), ( W.float 1.0e-7, Just (N 1.0e-7) ) ]
            , [ ( W.date d, Just (D d) ), ( W.dateTime dt, Just (DT dt) ), ( W.date (Date.fromCalendarDate 1900 Mar 1), Just (D (Date.fromCalendarDate 1900 Mar 1)) ) ]
            , [ ( W.error "#N/A", Just (Err "#N/A") ), ( W.error "#DIV/0!", Just (Err "#DIV/0!") ), ( W.blank, Nothing ), ( W.float 1.0e21, Just (N 1.0e21) ) ]
            , [ ( W.formulaWithResult "SUM(B1:C1)" (W.float -38.85841), Just (F "SUM(B1:C1)" (N -38.85841)) )
              , ( W.formulaWithResult "=A1&\"!\"" (W.string "text!"), Just (F "A1&\"!\"" (S "text!")) )
              , ( W.formulaWithResult "A2" (W.bool True), Just (F "A2" (B True)) )
              , ( W.formula "XLOOKUP(1,B1:C1,B1:C1)", Just (FNoValue "_xlfn.XLOOKUP(1,B1:C1,B1:C1)") )
              ]
            ]
          )
        ]


text : Sample
text =
    makeSample "text"
        [ ( "Text"
          , [ [ ( W.string "  leading and trailing  ", Just (S "  leading and trailing  ") ) ]
            , [ ( W.string "line\nbreak", Just (S "line\nbreak") ) ]
            , [ ( W.string "tab\there", Just (S "tab\there") ) ]
            , [ ( W.string "<xml> & \"quotes\" 'apos'", Just (S "<xml> & \"quotes\" 'apos'") ) ]
            , [ ( W.string "control\u{0001}char", Just (S "control\u{0001}char") ) ]
            , [ ( W.string "literal _x0041_ escape", Just (S "literal _x0041_ escape") ) ]
            , [ ( W.string "emoji 👍🏽 and CJK 日本語 and RTL שלום", Just (S "emoji 👍🏽 and CJK 日本語 and RTL שלום") ) ]
            , [ ( W.string (String.repeat 32767 "x"), Just (S (String.repeat 32767 "x")) ) ]
            , [ ( W.string "", Just (S "") ) ]
            , [ ( W.string "repeated", Just (S "repeated") ), ( W.string "repeated", Just (S "repeated") ) ]
            ]
          )
        ]


multiSheet : Sample
multiSheet =
    { name = "multi-sheet"
    , sheets =
        [ ( "First", [ [ ( W.string "one", Just (S "one") ) ] ] )
        , ( "Bob's Sheet", [ [ ( W.string "two", Just (S "two") ) ] ] )
        , ( "Empty", [] )
        , ( "Résumé & <Notes>", [ [ ( W.formulaWithResult "'Bob''s Sheet'!A1" (W.string "two"), Just (F "'Bob''s Sheet'!A1" (S "two")) ) ] ] )
        ]
    , active = Just "Bob's Sheet"
    , compression = W.Deflate
    }


{-| 150,000 cells: more than 4 MB of sheet XML, so `Auto` stores the sheet
uncompressed.
-}
large : Sample
large =
    makeSample "large"
        [ ( "Large"
          , List.range 1 15000
                |> List.map
                    (\r ->
                        List.range 1 10
                            |> List.map
                                (\c ->
                                    if r == 15000 && c == 10 then
                                        ( W.float 1.5e5, Just (N 1.5e5) )

                                    else if modBy 2 c == 0 then
                                        ( W.string ("row " ++ String.fromInt r), Nothing )

                                    else
                                        ( W.int (r * c), Nothing )
                                )
                    )
          )
        ]
