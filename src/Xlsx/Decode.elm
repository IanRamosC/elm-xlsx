module Xlsx.Decode exposing
    ( Decoder
    , string, int, float, bool, date, dateTime, value, cell
    , blank
    , column, field, optionalField, rowNumber
    , map, map2, map3, into, pipeline
    , oneOf, andThen, succeed, fail, fromResult, fromMaybe
    , FieldNames(..), decodeSheet
    , Error(..), DecodingError, Problem(..), errorToString
    )

{-| Turn the rows of a sheet into your own types, the way `Json.Decode` turns
JSON into values. The API follows
[`BrianHicks/elm-csv`](https://package.elm-lang.org/packages/BrianHicks/elm-csv/latest/Csv-Decode).

    import Xlsx.Decode as Decode exposing (Decoder)
    import Xlsx.Read as Read

    type alias Person =
        { name : String, age : Int, email : Maybe String }

    person : Decoder Person
    person =
        Decode.into Person
            |> Decode.pipeline (Decode.field "Name" Decode.string)
            |> Decode.pipeline (Decode.field "Age" Decode.int)
            |> Decode.pipeline (Decode.optionalField "Email" Decode.string)

    people : Bytes -> Result String (List Person)
    people bytes =
        Read.read bytes
            |> Result.andThen Read.firstSheet
            |> Result.mapError Read.errorToString
            |> Result.andThen
                (Decode.decodeSheet Decode.FieldNamesFromFirstRow person
                    >> Result.mapError Decode.errorToString
                )

Rows with no values at all are skipped. Every row is decoded, and all
failing rows are reported together, each with its row number and column.


# Decoders

@docs Decoder


## Values

@docs string, int, float, bool, date, dateTime, value, cell
@docs blank


## Where to look

@docs column, field, optionalField, rowNumber


## Combining

@docs map, map2, map3, into, pipeline
@docs oneOf, andThen, succeed, fail, fromResult, fromMaybe


# Running

@docs FieldNames, decodeSheet


# Errors

@docs Error, DecodingError, Problem, errorToString

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Xlsx.Date as XDate
import Xlsx.Read as Read exposing (CellValue(..))
import Xlsx.Ref as Ref



-- DECODERS


{-| Decodes one row into an `a`.
-}
type Decoder a
    = Decoder (State -> Result Failure a)


type alias State =
    { cells : Dict Int Read.Cell
    , lastColumn : Int
    , fieldNames : Dict String Int
    , row : Int
    , target : Maybe Int
    }


type Failure
    = Failure (Maybe Int) Problem
    | MissingField String


{-| Text cells. Numbers, booleans and dates are not turned into text: use
[`oneOf`](#oneOf) or [`value`](#value) when a column mixes types.
-}
string : Decoder String
string =
    fromValue "text" <|
        \v ->
            case v of
                Text s ->
                    Just s

                _ ->
                    Nothing


{-| Whole numbers. Text that holds a whole number (`" 42 "`), as left behind
by CSV imports, is accepted too.
-}
int : Decoder Int
int =
    fromValue "a whole number" <|
        \v ->
            case v of
                Number n ->
                    if n == toFloat (round n) && abs n <= 9007199254740991 then
                        Just (round n)

                    else
                        Nothing

                Text s ->
                    String.toInt (String.trim s)

                _ ->
                    Nothing


{-| Numbers. Text that holds a number is accepted too.
-}
float : Decoder Float
float =
    fromValue "a number" <|
        \v ->
            case v of
                Number n ->
                    Just n

                Text s ->
                    String.toFloat (String.trim s)

                _ ->
                    Nothing


{-| `TRUE`/`FALSE` cells, and the text `true` or `false` in any case.
-}
bool : Decoder Bool
bool =
    fromValue "TRUE or FALSE" <|
        \v ->
            case v of
                Boolean b ->
                    Just b

                Text s ->
                    case String.toLower (String.trim s) of
                        "true" ->
                            Just True

                        "false" ->
                            Just False

                        _ ->
                            Nothing

                _ ->
                    Nothing


{-| Dates. Also accepts a date-time at midnight, and ISO 8601 text
(`2024-02-29`). A plain number is refused: without a date format it is not
known to be a date.
-}
date : Decoder Date
date =
    fromValue "a date" <|
        \v ->
            case v of
                DateValue d ->
                    Just d

                DateTimeValue dt ->
                    if dt.hour == 0 && dt.minute == 0 && dt.second == 0 && dt.millisecond == 0 then
                        Just dt.date

                    else
                        Nothing

                Text s ->
                    Date.fromIsoString (String.trim s) |> Result.toMaybe

                _ ->
                    Nothing


{-| Date-times. A date alone is read as midnight.
-}
dateTime : Decoder XDate.DateTime
dateTime =
    fromValue "a date and time" <|
        \v ->
            case v of
                DateTimeValue dt ->
                    Just dt

                DateValue d ->
                    Just (XDate.midnight d)

                _ ->
                    Nothing


{-| The value as it is, whatever its type.
-}
value : Decoder CellValue
value =
    Decoder (\state -> Result.map .value (currentCell state))


{-| The whole cell, with its formula and number format.
-}
cell : Decoder Read.Cell
cell =
    Decoder currentCell


fromValue : String -> (CellValue -> Maybe a) -> Decoder a
fromValue expected convert =
    Decoder
        (\state ->
            currentCell state
                |> Result.andThen
                    (\c ->
                        case convert c.value of
                            Just a ->
                                Ok a

                            Nothing ->
                                -- without column or field, the one column is A
                                Err (Failure (Just (Maybe.withDefault 1 state.target)) (Expected expected c.value))
                    )
        )


currentCell : State -> Result Failure Read.Cell
currentCell state =
    case state.target of
        Just col ->
            Ok (cellAt col state)

        Nothing ->
            if state.lastColumn <= 1 then
                Ok (cellAt 1 state)

            else
                Err (Failure Nothing (ExpectedOneColumn state.lastColumn))


cellAt : Int -> State -> Read.Cell
cellAt col state =
    Dict.get col state.cells
        |> Maybe.withDefault { value = Empty, formula = Nothing, numberFormat = "General", hyperlink = Nothing }


{-| `Nothing` for an empty cell (or text that is only whitespace), otherwise
the decoder's result.

    Decode.field "Email" (Decode.blank Decode.string)

-}
blank : Decoder a -> Decoder (Maybe a)
blank (Decoder decode) =
    Decoder
        (\state ->
            case currentCell state of
                Ok c ->
                    case c.value of
                        Empty ->
                            Ok Nothing

                        Text s ->
                            if String.isEmpty (String.trim s) then
                                Ok Nothing

                            else
                                Result.map Just (decode state)

                        _ ->
                            Result.map Just (decode state)

                Err e ->
                    Err e
        )



-- LOCATION


{-| Decode the cell in a column, counting from 1 (`A`).

    Decode.column 2 Decode.int -- column B

-}
column : Int -> Decoder a -> Decoder a
column col (Decoder decode) =
    Decoder (\state -> decode { state | target = Just col })


{-| Decode the cell under a header. See [`FieldNames`](#FieldNames).
Header names are compared after trimming spaces, and are case-sensitive.
-}
field : String -> Decoder a -> Decoder a
field name (Decoder decode) =
    Decoder
        (\state ->
            case Dict.get (String.trim name) state.fieldNames of
                Just col ->
                    decode { state | target = Just col }

                Nothing ->
                    Err (MissingField name)
        )


{-| Like [`field`](#field), but `Nothing` when the header does not exist or
the cell is empty.
-}
optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField name decoder =
    Decoder
        (\state ->
            case Dict.get (String.trim name) state.fieldNames of
                Just col ->
                    let
                        (Decoder decode) =
                            blank decoder
                    in
                    decode { state | target = Just col }

                Nothing ->
                    Ok Nothing
        )


{-| The row number of the row being decoded, as Excel shows it.
-}
rowNumber : Decoder Int
rowNumber =
    Decoder (\state -> Ok state.row)



-- COMBINING


{-| Change the decoded value.

    Decode.map String.toUpper (Decode.field "Code" Decode.string)

-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decode) =
    Decoder (decode >> Result.map f)


{-| Decode two things from the same row and combine them.

    Decode.map2 Tuple.pair
        (Decode.field "Name" Decode.string)
        (Decode.field "Age" Decode.int)

-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f (Decoder a) (Decoder b) =
    Decoder (\state -> Result.map2 f (a state) (b state))


{-| Like [`map2`](#map2), for three things.
-}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 f (Decoder a) (Decoder b) (Decoder c) =
    Decoder (\state -> Result.map3 f (a state) (b state) (c state))


{-| Start a pipeline: `into Person |> pipeline (field "Name" string) |> ...`
-}
into : a -> Decoder a
into =
    succeed


{-| Add the next field to a decoder started with [`into`](#into):

    type alias Person =
        { name : String, age : Int }

    person : Decoder Person
    person =
        Decode.into Person
            |> Decode.pipeline (Decode.field "Name" Decode.string)
            |> Decode.pipeline (Decode.field "Age" Decode.int)

-}
pipeline : Decoder a -> Decoder (a -> b) -> Decoder b
pipeline next fn =
    map2 (\f a -> f a) fn next


{-| Try decoders in order and use the first that succeeds.

    -- an ID column that holds numbers in some rows and text in others
    Decode.oneOf (Decode.map String.fromInt Decode.int) [ Decode.string ]

-}
oneOf : Decoder a -> List (Decoder a) -> Decoder a
oneOf first rest =
    Decoder
        (\state ->
            let
                go decoders failures =
                    case decoders of
                        [] ->
                            case List.reverse failures of
                                [ single ] ->
                                    Err single

                                many ->
                                    case List.filter isMissingField many of
                                        missing :: _ ->
                                            Err missing

                                        [] ->
                                            Err (Failure state.target (NoneOf (List.filterMap failureProblem many)))

                        (Decoder decode) :: others ->
                            case decode state of
                                Ok a ->
                                    Ok a

                                Err failure ->
                                    go others (failure :: failures)
            in
            go (first :: rest) []
        )


isMissingField : Failure -> Bool
isMissingField failure =
    case failure of
        MissingField _ ->
            True

        Failure _ _ ->
            False


failureProblem : Failure -> Maybe ( Maybe Int, Problem )
failureProblem failure =
    case failure of
        Failure col problem ->
            Just ( col, problem )

        MissingField _ ->
            Nothing


{-| Choose the next decoder based on a decoded value, for example to
validate it (see [`fromResult`](#fromResult)) or to read a row
differently depending on a "Type" column.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f (Decoder decode) =
    Decoder
        (\state ->
            decode state
                |> Result.andThen
                    (\a ->
                        let
                            (Decoder next) =
                                f a
                        in
                        next state
                    )
        )


{-| A decoder that always gives this value and reads nothing.
-}
succeed : a -> Decoder a
succeed a =
    Decoder (\_ -> Ok a)


{-| Fail with a message, reported at the current row and column.
-}
fail : String -> Decoder a
fail message =
    Decoder (\state -> Err (Failure state.target (Failed message)))


{-| Turn a `Result` into a decoder, for validation:

    Decode.int
        |> Decode.andThen
            (\n ->
                Decode.fromResult
                    (if n >= 0 then
                        Ok n

                     else
                        Err "must not be negative"
                    )
            )

-}
fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok a ->
            succeed a

        Err message ->
            fail message


{-| Turn a `Maybe` into a decoder, failing with the message on `Nothing`:

    Decode.string
        |> Decode.andThen (\s -> Decode.fromMaybe "not a known country" (Dict.get s countries))

-}
fromMaybe : String -> Maybe a -> Decoder a
fromMaybe message maybe =
    case maybe of
        Just a ->
            succeed a

        Nothing ->
            fail message



-- RUNNING


{-| Where header names come from.

  - `NoFieldNames`: every row is data; use [`column`](#column).
  - `FieldNamesFromFirstRow`: the first row with any value is the header;
    data starts below it. Title rows above the header are not supported;
    use `CustomFieldNames` with the rows you want instead.
  - `CustomFieldNames`: names for columns A, B, C, …; every row is data.

-}
type FieldNames
    = NoFieldNames
    | FieldNamesFromFirstRow
    | CustomFieldNames (List String)


{-| Decode every non-empty row of a sheet.
-}
decodeSheet : FieldNames -> Decoder a -> Read.Sheet -> Result Error (List a)
decodeSheet fieldNames (Decoder decode) sheet =
    let
        sheetName =
            Read.name sheet

        rows =
            groupRows (Read.cells sheet)

        namesFrom list =
            list
                |> List.indexedMap (\i n -> ( String.trim n, i + 1 ))
                |> List.filter (\( n, _ ) -> not (String.isEmpty n))
                |> List.foldr (\( n, i ) acc -> Dict.insert n i acc) Dict.empty

        setup =
            case fieldNames of
                NoFieldNames ->
                    Ok ( Dict.empty, rows )

                CustomFieldNames names ->
                    Ok ( namesFrom names, rows )

                FieldNamesFromFirstRow ->
                    case rows of
                        ( _, header ) :: data ->
                            Ok
                                ( header
                                    |> Dict.toList
                                    |> List.filterMap
                                        (\( col, c ) ->
                                            case c.value of
                                                Text s ->
                                                    Just ( String.trim s, col )

                                                Number n ->
                                                    Just ( String.fromFloat n, col )

                                                _ ->
                                                    Nothing
                                        )
                                    |> List.filter (\( n, _ ) -> not (String.isEmpty n))
                                    |> List.foldr (\( n, i ) acc -> Dict.insert n i acc) Dict.empty
                                , data
                                )

                        [] ->
                            Err (NoHeaderRow { sheet = sheetName })
    in
    setup
        |> Result.andThen
            (\( names, data ) ->
                let
                    columnNames =
                        Dict.foldl (\n i acc -> Dict.insert i n acc) Dict.empty names

                    step ( r, cells ) ( oks, errs, missing ) =
                        case missing of
                            Just _ ->
                                ( oks, errs, missing )

                            Nothing ->
                                case
                                    decode
                                        { cells = cells
                                        , lastColumn = Dict.foldl (\c _ _ -> c) 0 cells
                                        , fieldNames = names
                                        , row = r
                                        , target = Nothing
                                        }
                                of
                                    Ok a ->
                                        ( a :: oks, errs, Nothing )

                                    Err (Failure col problem) ->
                                        ( oks
                                        , { row = r
                                          , column = col
                                          , field = col |> Maybe.andThen (\c -> Dict.get c columnNames)
                                          , problem = problem
                                          }
                                            :: errs
                                        , Nothing
                                        )

                                    Err (MissingField name) ->
                                        ( oks, errs, Just name )
                in
                case List.foldl step ( [], [], Nothing ) data of
                    ( _, _, Just name ) ->
                        Err (FieldNotFound { sheet = sheetName, field = name, available = Dict.keys names })

                    ( oks, [], Nothing ) ->
                        Ok (List.reverse oks)

                    ( _, errs, Nothing ) ->
                        Err (DecodingErrors { sheet = sheetName, errors = List.reverse errs })
            )


{-| Group the non-empty cells by row, in order.
-}
groupRows : List ( Ref.Ref, Read.Cell ) -> List ( Int, Dict Int Read.Cell )
groupRows cells =
    cells
        |> List.filter (\( _, c ) -> c.value /= Empty)
        |> List.foldr
            (\( ref, c ) acc ->
                case acc of
                    ( r, row ) :: rest ->
                        if r == ref.row then
                            ( r, Dict.insert ref.column c row ) :: rest

                        else
                            ( ref.row, Dict.singleton ref.column c ) :: acc

                    [] ->
                        [ ( ref.row, Dict.singleton ref.column c ) ]
            )
            []



-- ERRORS


{-| Why decoding failed.

  - `NoHeaderRow`: `FieldNamesFromFirstRow` on a sheet with no values.
  - `FieldNotFound`: a [`field`](#field) is not among the headers, which
    are listed in `available`.
  - `DecodingErrors`: every row that failed, in order.

-}
type Error
    = NoHeaderRow { sheet : String }
    | FieldNotFound { sheet : String, field : String, available : List String }
    | DecodingErrors { sheet : String, errors : List DecodingError }


{-| A row that failed. `column` is 1-based (`A` = 1), and `Nothing` when
the problem is not about one cell. `field` is the header of that column,
when there is one.
-}
type alias DecodingError =
    { row : Int
    , column : Maybe Int
    , field : Maybe String
    , problem : Problem
    }


{-| What was wrong.

  - `Expected what found`: the cell held something else, like
    `Expected "a whole number" (Text "n/a")`.
  - `ExpectedOneColumn n`: a value decoder was used without `column` or
    `field` on a row with `n` columns.
  - `Failed message`: from [`fail`](#fail) or [`fromResult`](#fromResult).
  - `NoneOf`: every decoder in a [`oneOf`](#oneOf) failed.

-}
type Problem
    = Expected String CellValue
    | ExpectedOneColumn Int
    | Failed String
    | NoneOf (List ( Maybe Int, Problem ))


{-| A message you can show to people. At most 10 rows are listed.

    Sheet "People", row 3, column C ("Age"): expected a whole number, found the text "n/a"

-}
errorToString : Error -> String
errorToString error =
    case error of
        NoHeaderRow { sheet } ->
            "Sheet \"" ++ sheet ++ "\" has no header row: it is empty."

        FieldNotFound details ->
            "Sheet \""
                ++ details.sheet
                ++ "\" has no column named \""
                ++ details.field
                ++ "\". "
                ++ (if List.isEmpty details.available then
                        "It has no column names."

                    else
                        "Its columns are: " ++ String.join ", " (List.map (\n -> "\"" ++ n ++ "\"") details.available) ++ "."
                   )

        DecodingErrors { sheet, errors } ->
            let
                shown =
                    List.take 10 errors

                more =
                    List.length errors - List.length shown
            in
            (List.map (decodingErrorToString sheet) shown
                ++ (if more > 0 then
                        [ "… and " ++ String.fromInt more ++ " more rows." ]

                    else
                        []
                   )
            )
                |> String.join "\n"


decodingErrorToString : String -> DecodingError -> String
decodingErrorToString sheet e =
    "Sheet \""
        ++ sheet
        ++ "\", row "
        ++ String.fromInt e.row
        ++ (case e.column of
                Just col ->
                    ", column "
                        ++ Ref.columnName col
                        ++ (case e.field of
                                Just f ->
                                    " (\"" ++ f ++ "\")"

                                Nothing ->
                                    ""
                           )

                Nothing ->
                    ""
           )
        ++ ": "
        ++ problemToString e.problem


problemToString : Problem -> String
problemToString problem =
    case problem of
        Expected what found ->
            "expected " ++ what ++ ", found " ++ describe found

        ExpectedOneColumn n ->
            "expected one column, found " ++ String.fromInt n ++ " (use column or field to pick one)"

        Failed message ->
            message

        NoneOf problems ->
            "none of the options worked: "
                ++ String.join "; "
                    (List.map
                        (\( col, p ) ->
                            (case col of
                                Just c ->
                                    "column " ++ Ref.columnName c ++ ": "

                                Nothing ->
                                    ""
                            )
                                ++ problemToString p
                        )
                        problems
                    )


describe : CellValue -> String
describe v =
    case v of
        Text s ->
            "the text \"" ++ s ++ "\""

        Number n ->
            "the number " ++ String.fromFloat n

        Boolean True ->
            "TRUE"

        Boolean False ->
            "FALSE"

        DateValue d ->
            "the date " ++ Date.toIsoString d

        DateTimeValue dt ->
            "the date and time " ++ Date.toIsoString dt.date ++ " " ++ pad dt.hour ++ ":" ++ pad dt.minute ++ ":" ++ pad dt.second

        ErrorValue e ->
            "the error " ++ e

        Empty ->
            "an empty cell"


pad : Int -> String
pad n =
    String.padLeft 2 '0' (String.fromInt n)
