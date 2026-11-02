module Xlsx.Internal.NumFmt exposing
    ( builtinId, builtinCode
    , isDateFormat, isDateFormatId
    , Kind(..), classify, classifyId
    , firstCustomId, defaultDate, defaultDateTime
    )

{-| Excel's number formats: the built-in table and date-format detection.

@docs builtinId, builtinCode
@docs isDateFormat, isDateFormatId
@docs Kind, classify, classifyId
@docs firstCustomId, defaultDate, defaultDateTime

-}

import Dict exposing (Dict)


{-| Custom formats are numbered from 164 up.
-}
firstCustomId : Int
firstCustomId =
    164


{-| The format given to date cells that have no format of their own.
-}
defaultDate : String
defaultDate =
    "yyyy-mm-dd"


{-| The format given to date-time cells that have no format of their own.
-}
defaultDateTime : String
defaultDateTime =
    "yyyy-mm-dd hh:mm:ss"


{-| The built-in formats every Excel install knows (ids 0–49, US English).
Ids 14 and 22 are shown according to the user's locale.
-}
builtins : List ( Int, String )
builtins =
    [ ( 0, "General" )
    , ( 1, "0" )
    , ( 2, "0.00" )
    , ( 3, "#,##0" )
    , ( 4, "#,##0.00" )
    , ( 9, "0%" )
    , ( 10, "0.00%" )
    , ( 11, "0.00E+00" )
    , ( 12, "# ?/?" )
    , ( 13, "# ??/??" )
    , ( 14, "m/d/yy" )
    , ( 15, "d-mmm-yy" )
    , ( 16, "d-mmm" )
    , ( 17, "mmm-yy" )
    , ( 18, "h:mm AM/PM" )
    , ( 19, "h:mm:ss AM/PM" )
    , ( 20, "h:mm" )
    , ( 21, "h:mm:ss" )
    , ( 22, "m/d/yy h:mm" )
    , ( 37, "#,##0 ;(#,##0)" )
    , ( 38, "#,##0 ;[Red](#,##0)" )
    , ( 39, "#,##0.00;(#,##0.00)" )
    , ( 40, "#,##0.00;[Red](#,##0.00)" )
    , ( 45, "mm:ss" )
    , ( 46, "[h]:mm:ss" )
    , ( 47, "mm:ss.0" )
    , ( 48, "##0.0E+0" )
    , ( 49, "@" )
    ]


byCode : Dict String Int
byCode =
    Dict.fromList (List.map (\( id, code ) -> ( code, id )) builtins)


byId : Dict Int String
byId =
    Dict.fromList builtins


{-| The built-in id of a format string, if it exactly matches one.
-}
builtinId : String -> Maybe Int
builtinId code =
    Dict.get code byCode


{-| The format string of a built-in id.
-}
builtinCode : Int -> Maybe String
builtinCode id =
    Dict.get id byId


{-| Whether a built-in format id shows a date or time. Includes the
locale-specific ids 27–36 and 50–58 used by East Asian versions of Excel.
-}
isDateFormatId : Int -> Bool
isDateFormatId id =
    (id >= 14 && id <= 22) || (id >= 45 && id <= 47) || (id >= 27 && id <= 36) || (id >= 50 && id <= 58)


{-| Whether a format string shows a date or time, following openpyxl and
SheetJS: look at the first section only, drop quoted text, `\x` escapes and
`[...]` blocks (except elapsed time `[h]`, `[mm]`, `[ss]`), then look for any
of `y m d h s`.

    isDateFormat "yyyy-mm-dd" == True

    isDateFormat "#,##0.00" == False

    isDateFormat "0.00\"h\"" == False

-}
isDateFormat : String -> Bool
isDateFormat code =
    if String.toLower code == "general" then
        False

    else
        significant code
            |> String.any (\c -> c == 'y' || c == 'm' || c == 'd' || c == 'h' || c == 's' || c == elapsedMarker)


{-| How a number with this format should be read.

  - `Date`: shows a date only (`yyyy-mm-dd`).
  - `DateTime`: shows a time, with or without a date (`h:mm`, `m/d/yy h:mm`).
  - `Elapsed`: a duration (`[h]:mm:ss`); the number is a count of days, not
    a point in time.
  - `Plain`: anything else.

-}
type Kind
    = Plain
    | Date
    | DateTime
    | Elapsed


{-| -}
classify : String -> Kind
classify code =
    if String.toLower code == "general" then
        Plain

    else
        let
            chars =
                significant code

            has c =
                String.any ((==) c) chars
        in
        if has elapsedMarker then
            Elapsed

        else if has 'h' || has 's' || String.contains "am/pm" chars || String.contains "a/p" chars then
            DateTime

        else if has 'y' || has 'm' || has 'd' then
            Date

        else
            Plain


{-| The kind of a built-in format id. The East Asian ids (27–36, 50–58) have
no fixed code; they are read as dates.
-}
classifyId : Int -> Kind
classifyId id =
    case builtinCode id of
        Just code ->
            classify code

        Nothing ->
            if isDateFormatId id then
                Date

            else
                Plain


{-| The lowercased characters of a format's first section that can carry
meaning for dates, with elapsed-time blocks replaced by a marker.
-}
significant : String -> String
significant code =
    stripFormat (String.toList code) []
        |> String.fromList
        |> String.toLower


elapsedMarker : Char
elapsedMarker =
    '!'


{-| Keep only the characters that matter for date detection, stopping at the
first section separator.
-}
stripFormat : List Char -> List Char -> List Char
stripFormat chars acc =
    case chars of
        [] ->
            List.reverse acc

        ';' :: _ ->
            List.reverse acc

        '"' :: rest ->
            stripFormat (dropAfter '"' rest) acc

        '\\' :: _ :: rest ->
            stripFormat rest acc

        '_' :: _ :: rest ->
            stripFormat rest acc

        '*' :: _ :: rest ->
            stripFormat rest acc

        '[' :: rest ->
            let
                inside =
                    takeUntil ']' rest

                lowered =
                    String.toLower (String.fromList inside)
            in
            if isElapsed lowered then
                stripFormat (dropAfter ']' rest) (elapsedMarker :: acc)

            else
                stripFormat (dropAfter ']' rest) acc

        c :: rest ->
            stripFormat rest (c :: acc)


isElapsed : String -> Bool
isElapsed s =
    not (String.isEmpty s) && (String.all ((==) 'h') s || String.all ((==) 'm') s || String.all ((==) 's') s)


dropAfter : Char -> List Char -> List Char
dropAfter stop chars =
    case chars of
        [] ->
            []

        c :: rest ->
            if c == stop then
                rest

            else
                dropAfter stop rest


takeUntil : Char -> List Char -> List Char
takeUntil stop chars =
    takeUntilHelp stop chars []


takeUntilHelp : Char -> List Char -> List Char -> List Char
takeUntilHelp stop chars acc =
    case chars of
        [] ->
            List.reverse acc

        c :: rest ->
            if c == stop then
                List.reverse acc

            else
                takeUntilHelp stop rest (c :: acc)
