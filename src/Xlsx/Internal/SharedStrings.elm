module Xlsx.Internal.SharedStrings exposing
    ( Table, empty, insert, isEmpty
    , toXml
    , parse
    )

{-| The shared strings table (`xl/sharedStrings.xml`). Every text cell points
at an entry by index, and repeated strings are stored once.

@docs Table, empty, insert, isEmpty
@docs toXml


# Reading

@docs parse

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Xlsx.Internal.Xml.Escape as Escape
import Xlsx.Internal.Xml.Scan as Scan


{-| -}
type Table
    = Table
        { indexes : Dict String Int
        , reversed : List String
        , unique : Int
        , count : Int
        }


{-| -}
empty : Table
empty =
    Table { indexes = Dict.empty, reversed = [], unique = 0, count = 0 }


{-| Whether no text cell has been added.
-}
isEmpty : Table -> Bool
isEmpty (Table t) =
    t.count == 0


{-| Add one use of a string and get its index.
-}
insert : String -> Table -> ( Int, Table )
insert s (Table t) =
    case Dict.get s t.indexes of
        Just index ->
            ( index, Table { t | count = t.count + 1 } )

        Nothing ->
            ( t.unique
            , Table
                { indexes = Dict.insert s t.unique t.indexes
                , reversed = s :: t.reversed
                , unique = t.unique + 1
                , count = t.count + 1
                }
            )


{-| The `sst` part.
-}
toXml : Table -> String
toXml (Table t) =
    String.concat
        ("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<sst xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" count=\""
            :: String.fromInt t.count
            :: "\" uniqueCount=\""
            :: String.fromInt t.unique
            :: "\">"
            :: List.foldl (\s acc -> item s :: acc) [ "</sst>" ] t.reversed
        )


item : String -> String
item s =
    if Escape.needsPreserve s then
        "<si><t xml:space=\"preserve\">" ++ Escape.text (Escape.encodeXString s) ++ "</t></si>"

    else
        "<si><t>" ++ Escape.text (Escape.encodeXString s) ++ "</t></si>"



-- READING


{-| The strings of an `sst` part, in order. Rich text runs are joined into
plain text; phonetic guides are left out.
-}
parse : String -> Array String
parse xml =
    let
        prefix =
            Scan.rootPrefix xml
    in
    Scan.elements prefix "si" xml
        |> List.map (Scan.textRuns prefix)
        |> Array.fromList
