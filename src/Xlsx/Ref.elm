module Xlsx.Ref exposing
    ( Ref, toString, fromString
    , columnName, columnIndex
    , Range, cell, range, rangeToString, rangeFromString, rangeContains, rangeSize
    , quoteSheetName, onSheet
    )

{-| Cell references in Excel's `A1` notation.

Coordinates are **1-based** everywhere in this package, just like in Excel:
`A1` is `{ column = 1, row = 1 }`.

    toString { column = 2, row = 3 }
    --> "B3"

    fromString "AA10"
    --> Just { column = 27, row = 10 }


# Cells

@docs Ref, toString, fromString


# Columns

@docs columnName, columnIndex


# Ranges

@docs Range, cell, range, rangeToString, rangeFromString, rangeContains, rangeSize


# Sheet-qualified references

@docs quoteSheetName, onSheet

-}


{-| A cell position. Both numbers start at 1.
-}
type alias Ref =
    { column : Int
    , row : Int
    }


{-| A rectangular block of cells, from its top-left to its bottom-right corner
(both included).
-}
type alias Range =
    { start : Ref
    , end : Ref
    }


{-| The largest row number Excel supports.
-}
maxRow : Int
maxRow =
    1048576


{-| The largest column number Excel supports (`XFD`).
-}
maxColumn : Int
maxColumn =
    16384


{-| Write a reference in `A1` notation.

    toString { column = 28, row = 5 }
    --> "AB5"

-}
toString : Ref -> String
toString ref =
    columnName ref.column ++ String.fromInt ref.row


{-| Read a reference in `A1` notation. Lowercase letters and `$` anchors
(`$B$3`) are accepted. References outside Excel's grid (past `XFD1048576`)
give `Nothing`.

    fromString "$b$3"
    --> Just { column = 2, row = 3 }

    fromString "A0"
    --> Nothing

-}
fromString : String -> Maybe Ref
fromString input =
    let
        chars =
            String.toList (String.filter ((/=) '$') input)

        letters =
            takeWhile Char.isAlpha chars

        digits =
            List.drop (List.length letters) chars
    in
    if List.isEmpty letters || List.isEmpty digits || not (List.all Char.isDigit digits) || List.head digits == Just '0' then
        Nothing

    else
        Maybe.map2 Ref
            (columnIndex (String.fromList letters))
            (String.toInt (String.fromList digits) |> Maybe.andThen (inside 1 maxRow))


{-| The letters of a column number: `1` is `"A"`, `27` is `"AA"`, `16384` is
`"XFD"`. Numbers below 1 give an empty string.

    columnName 703
    --> "AAA"

-}
columnName : Int -> String
columnName n =
    columnNameHelp n []


columnNameHelp : Int -> List Char -> String
columnNameHelp n acc =
    if n <= 0 then
        String.fromList acc

    else
        let
            m =
                modBy 26 (n - 1)
        in
        columnNameHelp ((n - 1 - m) // 26) (Char.fromCode (65 + m) :: acc)


{-| The number of a column from its letters, ignoring case. Columns past
`XFD` give `Nothing`.

    columnIndex "xfd"
    --> Just 16384

    columnIndex "XFE"
    --> Nothing

-}
columnIndex : String -> Maybe Int
columnIndex letters =
    if String.isEmpty letters || String.length letters > 3 then
        Nothing

    else
        String.foldl
            (\c acc ->
                Maybe.andThen
                    (\total ->
                        let
                            code =
                                Char.toCode (Char.toUpper c)
                        in
                        if code >= 65 && code <= 90 then
                            Just (total * 26 + code - 64)

                        else
                            Nothing
                    )
                    acc
            )
            (Just 0)
            letters
            |> Maybe.andThen (inside 1 maxColumn)


{-| A range that covers a single cell.
-}
cell : Ref -> Range
cell ref =
    { start = ref, end = ref }


{-| The range between two corners, given in any order.

    range { column = 3, row = 1 } { column = 1, row = 4 }
        |> rangeToString
    --> "A1:C4"

-}
range : Ref -> Ref -> Range
range a b =
    { start = { column = min a.column b.column, row = min a.row b.row }
    , end = { column = max a.column b.column, row = max a.row b.row }
    }


{-| Write a range as `A1:C4`. A single-cell range is written as `A1`.
-}
rangeToString : Range -> String
rangeToString r =
    if r.start == r.end then
        toString r.start

    else
        toString r.start ++ ":" ++ toString r.end


{-| Read a range such as `A1:C4`, `$A$1:$C$4` or a single cell `B2`. The
corners may be given in any order.

    rangeFromString "C4:A1"
    --> Just { start = { column = 1, row = 1 }, end = { column = 3, row = 4 } }

-}
rangeFromString : String -> Maybe Range
rangeFromString input =
    case String.split ":" input of
        [ single ] ->
            Maybe.map cell (fromString single)

        [ a, b ] ->
            Maybe.map2 range (fromString a) (fromString b)

        _ ->
            Nothing


{-| Whether a cell lies inside a range.
-}
rangeContains : Ref -> Range -> Bool
rangeContains ref r =
    ref.column >= r.start.column && ref.column <= r.end.column && ref.row >= r.start.row && ref.row <= r.end.row


{-| How many columns and rows a range spans.

    rangeSize (range { column = 1, row = 1 } { column = 3, row = 4 })
    --> { columns = 3, rows = 4 }

-}
rangeSize : Range -> { columns : Int, rows : Int }
rangeSize r =
    { columns = r.end.column - r.start.column + 1
    , rows = r.end.row - r.start.row + 1
    }


{-| Quote a sheet name for use in a formula or reference, when Excel requires
it. Quotes inside the name are doubled.

    quoteSheetName "Data"
    --> "Data"

    quoteSheetName "Q1 Sales"
    --> "'Q1 Sales'"

    quoteSheetName "Bob's"
    --> "'Bob''s'"

-}
quoteSheetName : String -> String
quoteSheetName name =
    if needsQuotes name then
        "'" ++ String.replace "'" "''" name ++ "'"

    else
        name


needsQuotes : String -> Bool
needsQuotes name =
    case String.uncons name of
        Nothing ->
            True

        Just ( first, _ ) ->
            Char.isDigit first
                || not (String.all (\c -> Char.isAlphaNum c || c == '_' || c == '.') name)
                || (fromString name /= Nothing)
                || looksLikeR1C1 name


looksLikeR1C1 : String -> Bool
looksLikeR1C1 name =
    let
        upper =
            String.toUpper name
    in
    upper == "R" || upper == "C" || (String.startsWith "R" upper && String.all Char.isDigit (String.dropLeft 1 upper))


{-| A range qualified with its sheet name, as used in formulas and internal
links.

    onSheet "Q1 Sales" (cell { column = 1, row = 1 })
    --> "'Q1 Sales'!A1"

-}
onSheet : String -> Range -> String
onSheet sheetName r =
    quoteSheetName sheetName ++ "!" ++ rangeToString r



-- HELPERS


inside : Int -> Int -> Int -> Maybe Int
inside low high n =
    if n >= low && n <= high then
        Just n

    else
        Nothing


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    takeWhileHelp predicate list []


takeWhileHelp : (a -> Bool) -> List a -> List a -> List a
takeWhileHelp predicate list acc =
    case list of
        x :: rest ->
            if predicate x then
                takeWhileHelp predicate rest (x :: acc)

            else
                List.reverse acc

        [] ->
            List.reverse acc
