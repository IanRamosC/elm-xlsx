module RoundTripTest exposing (suite)

{-| Random workbooks go through `Write.toBytes`, then `Read.read`, and must
come back with the same values.
-}

import Date
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)
import Time
import Xlsx.Date as XDate
import Xlsx.Read as Read exposing (CellValue(..))
import Xlsx.Write as W


{-| A cell to write, and the value expected back.
-}
cellFuzzer : Fuzzer ( W.Cell, CellValue )
cellFuzzer =
    Fuzz.oneOf
        [ Fuzz.map (\s -> ( W.string s, Text (normalizeNewlines s) )) Fuzz.string
        , Fuzz.map (\s -> ( W.string s, Text s )) (Fuzz.oneOfValues [ " leading", "trailing ", "\t", "_x0041_", "a\u{0001}b", "👍🏽🌈", "<&>\"'", "=SUM(A1)", "" ])
        , Fuzz.map (\n -> ( W.float n, Number n )) Fuzz.niceFloat
        , Fuzz.map (\n -> ( W.float n, Number n )) (Fuzz.oneOfValues [ 0, -0.0, 1.0e-300, 1.7976931348623157e308, -2.2250738585072014e-308, 0.1 + 0.2, 123456789012345680 ])
        , Fuzz.map (\n -> ( W.int n, Number (toFloat n) )) Fuzz.int
        , Fuzz.map (\b -> ( W.bool b, Boolean b )) Fuzz.bool
        , Fuzz.map (\d -> ( W.date d, DateValue d )) dateFuzzer
        , Fuzz.map (\dt -> ( W.dateTime dt, expectedDateTime dt )) dateTimeFuzzer
        , Fuzz.map (\code -> ( W.error code, ErrorValue code )) (Fuzz.oneOfValues [ "#NULL!", "#DIV/0!", "#VALUE!", "#REF!", "#NAME?", "#NUM!", "#N/A" ])
        , Fuzz.map (\n -> ( W.formulaWithResult "1+1" (W.float n), Number n )) Fuzz.niceFloat
        , Fuzz.constant ( W.blank, Empty )
        ]


{-| A date-time at midnight reads back as a date-time, not a date: the
format says it has a time.
-}
expectedDateTime : XDate.DateTime -> CellValue
expectedDateTime dt =
    DateTimeValue dt


normalizeNewlines : String -> String
normalizeNewlines s =
    s |> String.replace "\u{000D}\n" "\n" |> String.replace "\u{000D}" "\n"


dateFuzzer : Fuzzer Date.Date
dateFuzzer =
    Fuzz.intRange (Date.toRataDie XDate.minDate) (Date.toRataDie XDate.maxDate)
        |> Fuzz.map Date.fromRataDie


dateTimeFuzzer : Fuzzer XDate.DateTime
dateTimeFuzzer =
    Fuzz.map5 (\d h m s ms -> { date = d, hour = h, minute = m, second = s, millisecond = ms })
        dateFuzzer
        (Fuzz.intRange 0 23)
        (Fuzz.intRange 0 59)
        (Fuzz.intRange 0 59)
        (Fuzz.intRange 0 999)


sheetFuzzer : Fuzzer (List (List ( W.Cell, CellValue )))
sheetFuzzer =
    Fuzz.listOfLengthBetween 0 6 (Fuzz.listOfLengthBetween 0 6 cellFuzzer)


{-| What `Read.rows` gives back: trailing empty cells and rows dropped.
-}
expectedRows : List (List ( W.Cell, CellValue )) -> List (List CellValue)
expectedRows rows =
    rows
        |> List.map (List.map Tuple.second >> dropTrailing ((==) Empty))
        |> dropTrailing List.isEmpty


dropTrailing : (a -> Bool) -> List a -> List a
dropTrailing isEmpty list =
    List.foldr
        (\x acc ->
            if List.isEmpty acc && isEmpty x then
                []

            else
                x :: acc
        )
        []
        list


writeRead : List ( String, List (List ( W.Cell, CellValue )) ) -> Result String (List (List (List CellValue)))
writeRead sheets =
    W.workbook (List.map (\( name, rows ) -> W.sheet name (List.map (List.map Tuple.first >> W.row) rows)) sheets)
        |> W.toBytes { created = Time.millisToPosix 0, compression = W.Store }
        |> Result.mapError (List.map W.problemToString >> String.join "; ")
        |> Result.andThen
            (\bytes ->
                Read.read bytes
                    |> Result.andThen (\wb -> List.map (\( name, _ ) -> Read.sheet name wb |> Result.map Read.rows) sheets |> combine)
                    |> Result.mapError Read.errorToString
            )


combine : List (Result e a) -> Result e (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


suite : Test
suite =
    describe "write → read round trips"
        [ fuzz sheetFuzzer "one sheet" <|
            \rows ->
                writeRead [ ( "Data", rows ) ]
                    |> Expect.equal (Ok [ expectedRows rows ])
        , fuzz (Fuzz.listOfLengthBetween 1 4 sheetFuzzer) "several sheets" <|
            \sheets ->
                writeRead (List.indexedMap (\i rows -> ( "Sheet " ++ String.fromInt (i + 1), rows )) sheets)
                    |> Expect.equal (Ok (List.map expectedRows sheets))
        , fuzz Fuzz.string "any sheet name Excel accepts" <|
            \raw ->
                let
                    name =
                        raw
                            |> String.filter (\c -> not (List.member c [ ':', '\\', '/', '?', '*', '[', ']' ]) && Char.toCode c >= 32)
                            |> String.left 31
                            |> (\n ->
                                    if String.isEmpty (String.trim n) || String.startsWith "'" n || String.endsWith "'" n || String.toLower n == "history" then
                                        "x"

                                    else
                                        n
                               )
                in
                W.workbook [ W.sheet name [ W.row [ W.int 1 ] ] ]
                    |> W.toBytes W.defaultOptions
                    |> Result.mapError (List.map W.problemToString >> String.join "; ")
                    |> Result.andThen (Read.read >> Result.mapError Read.errorToString)
                    |> Result.map Read.sheetNames
                    |> Expect.equal (Ok [ name ])
        , test "200k cells" <|
            \_ ->
                let
                    cell i =
                        case modBy 4 i of
                            0 ->
                                ( W.int i, Number (toFloat i) )

                            1 ->
                                ( W.string ("s" ++ String.fromInt (modBy 1000 i)), Text ("s" ++ String.fromInt (modBy 1000 i)) )

                            2 ->
                                ( W.bool (modBy 8 i == 2), Boolean (modBy 8 i == 2) )

                            _ ->
                                ( W.float (toFloat i / 7), Number (toFloat i / 7) )

                    rows =
                        List.range 0 1999 |> List.map (\r -> List.range 0 99 |> List.map (\c -> cell (r * 100 + c)))
                in
                writeRead [ ( "Big", rows ) ]
                    |> Result.map (List.map (List.map List.length >> List.sum))
                    |> Expect.equal (Ok [ 200000 ])
        ]
