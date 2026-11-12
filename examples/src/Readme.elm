module Readme exposing (header, ordersFile, people, report, reportBytes)

{-| The code samples from the README, so the compiler checks them. Keep the
two in sync.
-}

import Bytes exposing (Bytes)
import Date exposing (Date)
import Time
import Xlsx.Decode as Decode exposing (Decoder)
import Xlsx.Encode as Encode
import Xlsx.Read as Read
import Xlsx.Style as Style
import Xlsx.Write as Write


type alias Order =
    { id : Int, customer : String, total : Float, placed : Date }


ordersFile : Time.Posix -> List Order -> Result (List Write.Problem) Bytes
ordersFile now orders =
    Write.workbook
        [ Encode.table "Orders"
            [ Encode.int "Order" .id
            , Encode.string "Customer" .customer
            , Encode.float "Total" .total
                |> Encode.style (Style.none |> Style.numberFormat (Style.currency "$" 2))
            , Encode.date "Placed" .placed
            ]
            orders
        ]
        |> Write.toBytes { created = now, compression = Write.Auto }


header : Style.Style
header =
    Style.none
        |> Style.bold
        |> Style.fill (Style.rgb 221 235 247)
        |> Style.borderBottom Style.Thin Style.black


report : Write.Workbook
report =
    Write.workbook
        [ Write.sheet "Summary"
            [ Write.row (List.map (Write.string >> Write.withStyle header) [ "Region", "Sales" ])
            , Write.row [ Write.string "North", Write.float 1250.5 ]
            , Write.row [ Write.string "South", Write.float 980 ]
            , Write.emptyRow
            , Write.row
                [ Write.string "Total"
                , Write.formula "SUM(B2:B3)"
                ]
            , Write.row [ Write.hyperlink "https://example.com/report" "Full report" ]
            ]
            |> Write.columnWidths [ 18, 12 ]
            |> Write.freeze { rows = 1, columns = 0 }
        ]
        |> Write.withProperties [ Write.title "Sales", Write.author "Finance" ]


reportBytes : Result (List String) Bytes
reportBytes =
    case Write.toBytes Write.defaultOptions report of
        Ok bytes ->
            Ok bytes

        Err problems ->
            Err (List.map Write.problemToString problems)


type alias Person =
    { name : String, age : Int, email : Maybe String }


person : Decoder Person
person =
    Decode.into Person
        |> Decode.pipeline (Decode.field "Name" Decode.string)
        |> Decode.pipeline (Decode.field "Age" Decode.int)
        |> Decode.pipeline (Decode.optionalField "Email" Decode.string)


people : Read.Sheet -> Result Decode.Error (List Person)
people =
    Decode.decodeSheet Decode.FieldNamesFromFirstRow person
