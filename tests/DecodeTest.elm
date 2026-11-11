module DecodeTest exposing (suite)

import Date
import Expect
import Test exposing (Test, describe, test)
import Time exposing (Month(..))
import Xlsx.Date as XDate
import Xlsx.Decode as Decode exposing (Decoder, FieldNames(..), Problem(..))
import Xlsx.Read as Read exposing (CellValue(..))
import Xlsx.Write as W


sheetOf : List W.Row -> Read.Sheet
sheetOf rows =
    case
        W.toBytes { created = Time.millisToPosix 0, compression = W.Store } (W.workbook [ W.sheet "People" rows ])
            |> Result.mapError (List.map W.problemToString >> String.join "; ")
            |> Result.andThen (Read.read >> Result.andThen Read.firstSheet >> Result.mapError Read.errorToString)
    of
        Ok s ->
            s

        Err e ->
            Debug.todo e


people : Read.Sheet
people =
    sheetOf
        [ W.row [ W.string "Name", W.string "Age", W.string " Email ", W.string "Born", W.string "Active" ]
        , W.row [ W.string "Ada", W.int 36, W.string "ada@example.com", W.date (Date.fromCalendarDate 1915 Dec 10), W.bool True ]
        , W.emptyRow
        , W.row [ W.string "Alan", W.string " 41 ", W.blank, W.string "1912-06-23", W.string "false" ]
        ]


type alias Person =
    { name : String, age : Int, email : Maybe String, born : Date.Date, active : Bool }


person : Decoder Person
person =
    Decode.into Person
        |> Decode.pipeline (Decode.field "Name" Decode.string)
        |> Decode.pipeline (Decode.field "Age" Decode.int)
        |> Decode.pipeline (Decode.optionalField "Email" Decode.string)
        |> Decode.pipeline (Decode.field "Born" Decode.date)
        |> Decode.pipeline (Decode.field "Active" Decode.bool)


suite : Test
suite =
    describe "Xlsx.Decode"
        [ test "pipeline with headers; empty rows skipped; lenient text numbers" <|
            \_ ->
                Decode.decodeSheet FieldNamesFromFirstRow person people
                    |> Expect.equal
                        (Ok
                            [ Person "Ada" 36 (Just "ada@example.com") (Date.fromCalendarDate 1915 Dec 10) True
                            , Person "Alan" 41 Nothing (Date.fromCalendarDate 1912 Jun 23) False
                            ]
                        )
        , test "columns without headers, and row numbers" <|
            \_ ->
                Decode.decodeSheet NoFieldNames (Decode.map2 Tuple.pair Decode.rowNumber (Decode.column 1 Decode.string)) people
                    |> Expect.equal (Ok [ ( 1, "Name" ), ( 2, "Ada" ), ( 4, "Alan" ) ])
        , test "custom field names" <|
            \_ ->
                Decode.decodeSheet (CustomFieldNames [ "n", "a" ]) (Decode.field "n" Decode.string) people
                    |> Result.map (List.take 1)
                    |> Expect.equal (Ok [ "Name" ])
        , test "errors carry row, column and header, and every failing row is reported" <|
            \_ ->
                Decode.decodeSheet FieldNamesFromFirstRow (Decode.field "Name" Decode.int) people
                    |> Expect.equal
                        (Err
                            (Decode.DecodingErrors
                                { sheet = "People"
                                , errors =
                                    [ { row = 2, column = Just 1, field = Just "Name", problem = Expected "a whole number" (Text "Ada") }
                                    , { row = 4, column = Just 1, field = Just "Name", problem = Expected "a whole number" (Text "Alan") }
                                    ]
                                }
                            )
                        )
        , test "error message" <|
            \_ ->
                Decode.decodeSheet FieldNamesFromFirstRow (Decode.field "Age" Decode.bool) people
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal (Err "Sheet \"People\", row 2, column B (\"Age\"): expected TRUE or FALSE, found the number 36\nSheet \"People\", row 4, column B (\"Age\"): expected TRUE or FALSE, found the text \" 41 \"")
        , test "unknown field is reported once, with the available names" <|
            \_ ->
                Decode.decodeSheet FieldNamesFromFirstRow (Decode.field "Phone" Decode.string) people
                    |> Expect.equal (Err (Decode.FieldNotFound { sheet = "People", field = "Phone", available = [ "Active", "Age", "Born", "Email", "Name" ] }))
        , test "one-column decoder on a wide row" <|
            \_ ->
                Decode.decodeSheet NoFieldNames Decode.string people
                    |> Result.mapError
                        (\e ->
                            case e of
                                Decode.DecodingErrors { errors } ->
                                    List.take 1 errors |> List.map .problem

                                _ ->
                                    []
                        )
                    |> Expect.equal (Err [ ExpectedOneColumn 5 ])
        , test "oneOf, andThen, fromResult" <|
            \_ ->
                let
                    age =
                        Decode.oneOf Decode.int [ Decode.string |> Decode.andThen (\_ -> Decode.succeed -1) ]
                            |> Decode.andThen
                                (\n ->
                                    Decode.fromResult
                                        (if n < 40 then
                                            Ok n

                                         else
                                            Err "too old"
                                        )
                                )
                in
                Decode.decodeSheet NoFieldNames (Decode.column 2 age) people
                    |> Expect.equal
                        (Err
                            (Decode.DecodingErrors
                                { sheet = "People"
                                , errors = [ { row = 4, column = Just 2, field = Nothing, problem = Failed "too old" } ]
                                }
                            )
                        )
        , test "dates: date-times at midnight, and dateTime from a date" <|
            \_ ->
                sheetOf
                    [ W.row [ W.dateTime (XDate.midnight (Date.fromCalendarDate 2024 Jan 1)), W.date (Date.fromCalendarDate 2024 Jan 2) ] ]
                    |> Decode.decodeSheet NoFieldNames (Decode.map2 Tuple.pair (Decode.column 1 Decode.date) (Decode.column 2 Decode.dateTime))
                    |> Expect.equal (Ok [ ( Date.fromCalendarDate 2024 Jan 1, XDate.midnight (Date.fromCalendarDate 2024 Jan 2) ) ])
        , test "blank on an error cell still fails" <|
            \_ ->
                sheetOf [ W.row [ W.error "#N/A" ] ]
                    |> Decode.decodeSheet NoFieldNames (Decode.blank Decode.float)
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal (Err "Sheet \"People\", row 1, column A: expected a number, found the error #N/A")
        , test "empty sheet with header names" <|
            \_ ->
                Decode.decodeSheet FieldNamesFromFirstRow Decode.string (sheetOf [])
                    |> Expect.equal (Err (Decode.NoHeaderRow { sheet = "People" }))
        , test "numeric headers" <|
            \_ ->
                sheetOf [ W.row [ W.int 2023, W.int 2024 ], W.row [ W.int 1, W.int 2 ] ]
                    |> Decode.decodeSheet FieldNamesFromFirstRow (Decode.field "2024" Decode.int)
                    |> Expect.equal (Ok [ 2 ])
        ]
