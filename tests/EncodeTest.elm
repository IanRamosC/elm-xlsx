module EncodeTest exposing (suite)

import Date
import Expect
import Helpers exposing (contains, containsAll)
import Test exposing (Test, describe, test)
import Time exposing (Month(..))
import Xlsx.Encode as Encode
import Xlsx.Style as Style
import Xlsx.Write as W


type alias Person =
    { name : String, age : Int, score : Float, active : Bool, born : Date.Date }


people : List Person
people =
    [ { name = "Grace Hopper", age = 85, score = 99.5, active = True, born = Date.fromCalendarDate 1906 Dec 9 }
    , { name = "Alan Turing", age = 41, score = 97.25, active = False, born = Date.fromCalendarDate 1912 Jun 23 }
    ]


columns : List (Encode.Column Person)
columns =
    [ Encode.string "Name" .name
    , Encode.int "Age" .age
    , Encode.float "Score" .score |> Encode.style (Style.none |> Style.numberFormat (Style.decimal 2))
    , Encode.bool "Active" .active
    , Encode.date "Born" .born |> Encode.width 14
    ]


sheet1 : String
sheet1 =
    "xl/worksheets/sheet1.xml"


suite : Test
suite =
    describe "Xlsx.Encode"
        [ test "header row, data rows, frozen header and autofilter" <|
            \_ ->
                W.workbook [ Encode.table "People" columns people ]
                    |> containsAll sheet1
                        [ "<pane ySplit=\"1\" topLeftCell=\"A2\" activePane=\"bottomLeft\" state=\"frozen\"/>"
                        , "<row r=\"1\" spans=\"1:5\"><c r=\"A1\" s=\"1\" t=\"s\"><v>0</v></c><c r=\"B1\" s=\"1\" t=\"s\"><v>1</v></c>"
                        , "<row r=\"2\" spans=\"1:5\"><c r=\"A2\" t=\"s\"><v>5</v></c><c r=\"B2\"><v>85</v></c><c r=\"C2\" s=\"2\"><v>99.5</v></c><c r=\"D2\" t=\"b\"><v>1</v></c><c r=\"E2\" s=\"3\"><v>2535</v></c></row>"
                        , "<c r=\"C1\" s=\"1\" t=\"s\"><v>2</v></c>"
                        , "<autoFilter ref=\"A1:E3\"/>"
                        ]
        , test "header style" <|
            \_ ->
                W.workbook [ Encode.table "People" columns people ]
                    |> contains "xl/styles.xml" "<xf numFmtId=\"0\" fontId=\"1\" fillId=\"0\" borderId=\"1\" xfId=\"0\" applyFont=\"1\" applyBorder=\"1\"/>"
        , test "widths fit the content unless set" <|
            \_ ->
                W.workbook [ Encode.table "People" columns people ]
                    |> contains sheet1 "<cols><col min=\"1\" max=\"1\" width=\"14.7109375\" customWidth=\"1\"/><col min=\"2\" max=\"2\" width=\"8.7109375\" customWidth=\"1\"/><col min=\"3\" max=\"3\" width=\"10.7109375\" customWidth=\"1\"/><col min=\"4\" max=\"4\" width=\"11.7109375\" customWidth=\"1\"/><col min=\"5\" max=\"5\" width=\"14.7109375\" customWidth=\"1\"/></cols>"
        , test "a cell's own style wins over the column style" <|
            \_ ->
                W.workbook
                    [ Encode.table "T"
                        [ Encode.column "X" (\n -> W.float n |> W.withStyle (Style.none |> Style.numberFormat Style.integer))
                            |> Encode.style (Style.none |> Style.numberFormat (Style.decimal 2) |> Style.bold)
                        ]
                        [ 1.5 ]
                    ]
                    |> contains "xl/styles.xml" "<xf numFmtId=\"1\" fontId=\"1\""
        , test "options" <|
            \_ ->
                let
                    defaults =
                        Encode.defaultTableOptions
                in
                case W.toBytes W.defaultOptions (W.workbook [ Encode.tableWith { defaults | freezeHeader = False, autoFilter = False } "P" columns people ]) |> Result.toMaybe |> Maybe.andThen (Helpers.part sheet1) of
                    Just xml ->
                        Expect.equal ( False, False ) ( String.contains "<pane" xml, String.contains "<autoFilter" xml )

                    Nothing ->
                        Expect.fail "not written"
        , test "no records still gives a header" <|
            \_ ->
                W.workbook [ Encode.table "Empty" columns [] ]
                    |> containsAll sheet1 [ "<dimension ref=\"A1:E1\"/>", "<autoFilter ref=\"A1:E1\"/>" ]
        ]
