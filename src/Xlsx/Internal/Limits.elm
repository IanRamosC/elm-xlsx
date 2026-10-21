module Xlsx.Internal.Limits exposing
    ( maxRows, maxColumns, maxTextLength, maxFormulaLength
    , checkWorkbook, checkSheetName, checkCell
    , errorCodes
    )

{-| Excel's limits, and the checks `Xlsx.Write` runs before writing. Each
check returns every problem it finds instead of stopping at the first.

@docs maxRows, maxColumns, maxTextLength, maxFormulaLength
@docs checkWorkbook, checkSheetName, checkCell
@docs errorCodes

-}

import Date
import Dict
import Xlsx.Date
import Xlsx.Internal.Model exposing (Problem, Value(..))


{-| -}
maxRows : Int
maxRows =
    1048576


{-| -}
maxColumns : Int
maxColumns =
    16384


{-| Characters per cell, counted in UTF-16 code units as Excel does (and as
Elm's `String.length` does).
-}
maxTextLength : Int
maxTextLength =
    32767


{-| -}
maxFormulaLength : Int
maxFormulaLength =
    8192


{-| The seven error values a cell can hold.
-}
errorCodes : List String
errorCodes =
    [ "#NULL!", "#DIV/0!", "#VALUE!", "#REF!", "#NAME?", "#NUM!", "#N/A" ]


{-| Workbook-wide checks: at least one sheet, valid and unique sheet names, and
an active sheet that exists.
-}
checkWorkbook : { sheetNames : List String, activeSheet : Maybe String } -> List Problem
checkWorkbook { sheetNames, activeSheet } =
    let
        noSheets =
            if List.isEmpty sheetNames then
                [ workbookProblem "A workbook needs at least one sheet." ]

            else
                []

        duplicates =
            sheetNames
                |> List.foldl
                    (\name ( seen, acc ) ->
                        let
                            key =
                                String.toLower name
                        in
                        if Dict.member key seen then
                            ( seen, { sheet = Just name, cell = Nothing, message = "Another sheet already has this name (names are compared ignoring case)." } :: acc )

                        else
                            ( Dict.insert key () seen, acc )
                    )
                    ( Dict.empty, [] )
                |> Tuple.second
                |> List.reverse

        active =
            case activeSheet of
                Just name ->
                    if List.member name sheetNames then
                        []

                    else
                        [ workbookProblem ("The active sheet \"" ++ name ++ "\" is not in the workbook.") ]

                Nothing ->
                    []
    in
    noSheets ++ List.concatMap checkSheetName sheetNames ++ duplicates ++ active


workbookProblem : String -> Problem
workbookProblem message =
    { sheet = Nothing, cell = Nothing, message = message }


{-| A sheet name must have 1–31 characters, none of `: \ / ? * [ ]`, must not
start or end with an apostrophe, and must not be `History` (reserved by
Excel).
-}
checkSheetName : String -> List Problem
checkSheetName name =
    let
        problem message =
            [ { sheet = Just name, cell = Nothing, message = message } ]

        forbidden =
            String.filter (\c -> List.member c [ ':', '\\', '/', '?', '*', '[', ']' ]) name
    in
    if String.isEmpty name then
        problem "A sheet name cannot be empty."

    else if String.length name > 31 then
        problem "A sheet name can have at most 31 characters."

    else if not (String.isEmpty forbidden) then
        problem ("A sheet name cannot contain " ++ String.join " " (List.map String.fromChar (uniqueChars forbidden)) ++ ".")

    else if String.startsWith "'" name || String.endsWith "'" name then
        problem "A sheet name cannot start or end with an apostrophe."

    else if String.toLower name == "history" then
        problem "\"History\" is reserved by Excel."

    else
        []


uniqueChars : String -> List Char
uniqueChars s =
    String.foldr
        (\c acc ->
            if List.member c acc then
                acc

            else
                c :: acc
        )
        []
        s


{-| Problems with a single cell value, if any. `cell` is its `A1` reference.
-}
checkCell : String -> String -> Value -> List Problem
checkCell sheetName cellRef value =
    let
        problem message =
            [ { sheet = Just sheetName, cell = Just cellRef, message = message } ]
    in
    case value of
        Text s ->
            if String.length s > maxTextLength then
                problem ("Text is longer than Excel's limit of " ++ String.fromInt maxTextLength ++ " characters.")

            else
                []

        Number n ->
            checkNumber problem n

        DateValue d ->
            checkDate problem d

        DateTimeValue dt ->
            checkDate problem dt.date

        ErrorValue code ->
            checkErrorCode problem code

        Formula f cached ->
            (if String.isEmpty (String.trim f) || String.trim f == "=" then
                problem "A formula cannot be empty."

             else if String.length f > maxFormulaLength then
                problem ("A formula is longer than Excel's limit of " ++ String.fromInt maxFormulaLength ++ " characters.")

             else
                []
            )
                ++ (case cached of
                        Just inner ->
                            checkCell sheetName cellRef inner

                        Nothing ->
                            []
                   )

        Boolean _ ->
            []

        Blank ->
            []


checkNumber : (String -> List Problem) -> Float -> List Problem
checkNumber problem n =
    if isNaN n then
        problem "NaN cannot be stored in a cell."

    else if isInfinite n then
        problem "Infinity cannot be stored in a cell."

    else
        []


checkDate : (String -> List Problem) -> Date.Date -> List Problem
checkDate problem d =
    if Date.compare d Xlsx.Date.minDate == LT || Date.compare d Xlsx.Date.maxDate == GT then
        problem ("Excel only supports dates from 1900-01-01 to 9999-12-31, got " ++ Date.toIsoString d ++ ".")

    else
        []


checkErrorCode : (String -> List Problem) -> String -> List Problem
checkErrorCode problem code =
    if List.member code errorCodes then
        []

    else
        problem ("\"" ++ code ++ "\" is not an Excel error value. Use one of " ++ String.join " " errorCodes ++ ".")
