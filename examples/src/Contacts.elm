module Contacts exposing (Contact, decoder, export, sample)

{-| A contact list: how to read one from a sheet, and how to write one.
-}

import Date exposing (Date)
import Time exposing (Month(..))
import Xlsx.Date
import Xlsx.Decode as Decode exposing (Decoder)
import Xlsx.Encode as Encode
import Xlsx.Style as Style
import Xlsx.Write as Write


type alias Contact =
    { name : String
    , email : Maybe String
    , age : Int
    , joined : Date
    }



-- DECODING


{-| The headers can be in any order, and other columns are ignored.
-}
decoder : Decoder Contact
decoder =
    Decode.into Contact
        |> Decode.pipeline (Decode.field "Name" Decode.string)
        |> Decode.pipeline (Decode.optionalField "Email" Decode.string)
        |> Decode.pipeline (Decode.field "Age" Decode.int)
        |> Decode.pipeline (Decode.field "Joined" Decode.date)



-- WRITING


contactColumns : List (Encode.Column Contact)
contactColumns =
    [ Encode.string "Name" .name
    , Encode.column "Email" (\c -> Maybe.withDefault Write.blank (Maybe.map Write.string c.email))
    , Encode.int "Age" .age
    , Encode.date "Joined" .joined
    ]


{-| A file to try the app with. Two rows have mistakes on purpose, to show
the error messages: an age written in words, and a date with a time.
-}
sample : Write.Workbook
sample =
    Write.workbook
        [ Write.sheet "Contacts"
            [ Write.row (List.map (Write.string >> Write.withStyle (Style.bold Style.none)) [ "Name", "Email", "Age", "Joined" ])
            , Write.row [ Write.string "Ada Lovelace", Write.hyperlink "mailto:ada@example.com" "ada@example.com", Write.int 36, Write.date (Date.fromCalendarDate 2021 Dec 10) ]
            , Write.row [ Write.string "Alan Turing", Write.blank, Write.int 41, Write.date (Date.fromCalendarDate 2022 Jun 23) ]
            , Write.row [ Write.string "Grace Hopper", Write.string "grace@example.com", Write.string "eighty-five", Write.date (Date.fromCalendarDate 2023 Dec 9) ]
            , Write.row [ Write.string "Katherine Johnson", Write.string "katherine@example.com", Write.int 101, Write.dateTime (Xlsx.Date.dateTime (Date.fromCalendarDate 2024 Aug 26) 9 30 0) ]
            ]
            |> Write.columnWidths [ 20, 24, 8, 18 ]
            |> Write.freeze { rows = 1, columns = 0 }
        , Write.sheet "Notes"
            [ Write.row [ Write.string "Made with elm-xlsx." ]
            , Write.row [ Write.string "Total age", Write.formulaWithResult "SUM(Contacts!C2:C5)" (Write.int 178) ]
            ]
        ]
        |> Write.withProperties [ Write.title "Contacts", Write.author "elm-xlsx example" ]


export : List Contact -> Write.Workbook
export contacts =
    Write.workbook [ Encode.table "Contacts" contactColumns contacts ]
