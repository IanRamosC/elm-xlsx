port module Samples exposing (main)

{-| Sample workbooks for external validation. `run.mjs` writes each one to
`scripts/out/<name>.xlsx`, plus `<name>.json` listing the cells it contains,
and `scripts/validate.py` checks them with openpyxl and python-calamine.
-}

import Base64
import Date exposing (Date)
import Json.Encode as E
import Time exposing (Month(..))
import Xlsx.Date as XDate
import Xlsx.Encode as Encode
import Xlsx.Ref as Ref
import Xlsx.Style as Style
import Xlsx.Write as W


port start : (() -> msg) -> Sub msg


port emit : E.Value -> Cmd msg


main : Program () () ()
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \_ m -> ( m, Cmd.batch (List.map (emitSample >> emit) samples ++ List.map (emitGolden >> emit) goldens) )
        , subscriptions = \_ -> start identity
        }


{-| A cell we expect to read back: its kind and value as JSON.
-}
type Expected
    = S String
    | N Float
    | B Bool
    | D Date
    | DT XDate.DateTime
    | Err String
    | F String Expected
    | FNoValue String


type alias Sample =
    { name : String
    , sheets : List ( String, List (List ( W.Cell, Maybe Expected )) )
    , active : Maybe String
    , compression : W.Compression
    }


emitSample : Sample -> E.Value
emitSample sample =
    let
        wb =
            W.workbook
                (List.map
                    (\( name, rows ) -> W.sheet name (List.map (List.map Tuple.first >> W.row) rows))
                    sample.sheets
                )
                |> (case sample.active of
                        Just a ->
                            W.withActiveSheet a

                        Nothing ->
                            identity
                   )

        expected =
            sample.sheets
                |> List.concatMap
                    (\( name, rows ) ->
                        rows
                            |> List.indexedMap
                                (\r cells ->
                                    cells
                                        |> List.indexedMap
                                            (\c ( _, exp ) ->
                                                exp
                                                    |> Maybe.map
                                                        (\e ->
                                                            E.object
                                                                [ ( "sheet", E.string name )
                                                                , ( "ref", E.string (Ref.toString { column = c + 1, row = r + 1 }) )
                                                                , ( "expect", encodeExpected e )
                                                                ]
                                                        )
                                            )
                                        |> List.filterMap identity
                                )
                            |> List.concat
                    )
    in
    case W.toBytes { created = Time.millisToPosix 1794528000000, compression = sample.compression } wb of
        Ok bytes ->
            E.object
                [ ( "name", E.string sample.name )
                , ( "base64", E.string (Maybe.withDefault "" (Base64.fromBytes bytes)) )
                , ( "cells", E.list identity expected )
                , ( "sheets", E.list (Tuple.first >> E.string) sample.sheets )
                , ( "active", E.string (Maybe.withDefault (Maybe.withDefault "" (List.head (List.map Tuple.first sample.sheets))) sample.active) )
                ]

        Result.Err problems ->
            E.object
                [ ( "name", E.string sample.name )
                , ( "problems", E.list (W.problemToString >> E.string) problems )
                ]


{-| A workbook that rebuilds an XlsxWriter reference file; scripts/golden.py
compares the two.
-}
emitGolden : ( String, W.Workbook ) -> E.Value
emitGolden ( name, wb ) =
    case W.toBytes { created = Time.millisToPosix 1794528000000, compression = W.Auto } wb of
        Ok bytes ->
            E.object
                [ ( "name", E.string name )
                , ( "base64", E.string (Maybe.withDefault "" (Base64.fromBytes bytes)) )
                , ( "golden", E.bool True )
                ]

        Result.Err problems ->
            E.object
                [ ( "name", E.string name )
                , ( "problems", E.list (W.problemToString >> E.string) problems )
                ]


encodeExpected : Expected -> E.Value
encodeExpected e =
    case e of
        S s ->
            E.object [ ( "type", E.string "string" ), ( "value", E.string s ) ]

        N n ->
            E.object [ ( "type", E.string "number" ), ( "value", E.float n ) ]

        B b ->
            E.object [ ( "type", E.string "bool" ), ( "value", E.bool b ) ]

        D d ->
            E.object [ ( "type", E.string "date" ), ( "value", E.string (Date.toIsoString d) ) ]

        DT dt ->
            E.object
                [ ( "type", E.string "datetime" )
                , ( "value"
                  , E.string
                        (Date.toIsoString dt.date
                            ++ "T"
                            ++ pad dt.hour
                            ++ ":"
                            ++ pad dt.minute
                            ++ ":"
                            ++ pad dt.second
                            ++ "."
                            ++ String.padLeft 3 '0' (String.fromInt dt.millisecond)
                        )
                  )
                ]

        Err code ->
            E.object [ ( "type", E.string "error" ), ( "value", E.string code ) ]

        F f cached ->
            E.object [ ( "type", E.string "formula" ), ( "formula", E.string f ), ( "cached", encodeExpected cached ) ]

        FNoValue f ->
            E.object [ ( "type", E.string "formula" ), ( "formula", E.string f ), ( "cached", E.null ) ]


pad : Int -> String
pad n =
    String.padLeft 2 '0' (String.fromInt n)



-- SAMPLES


samples : List Sample
samples =
    [ simple, types, text, multiSheet, large ]


makeSample : String -> List ( String, List (List ( W.Cell, Maybe Expected )) ) -> Sample
makeSample name sheets =
    { name = name, sheets = sheets, active = Nothing, compression = W.Auto }


{-| Same content as XlsxWriter's `simple01` comparison file.
-}
simple : Sample
simple =
    makeSample "simple"
        [ ( "Sheet1"
          , [ [ ( W.string "Hello", Just (S "Hello") ) ]
            , [ ( W.int 123, Just (N 123) ) ]
            ]
          )
        ]


types : Sample
types =
    let
        d =
            Date.fromCalendarDate 2026 Nov 13

        dt =
            XDate.dateTime (Date.fromCalendarDate 2000 Feb 29) 13 45 30
    in
    makeSample "types"
        [ ( "Types"
          , [ [ ( W.string "text", Just (S "text") ), ( W.float 3.14159, Just (N 3.14159) ), ( W.int -42, Just (N -42) ) ]
            , [ ( W.bool True, Just (B True) ), ( W.bool False, Just (B False) ), ( W.float 1.0e-7, Just (N 1.0e-7) ) ]
            , [ ( W.date d, Just (D d) ), ( W.dateTime dt, Just (DT dt) ), ( W.date (Date.fromCalendarDate 1900 Mar 1), Just (D (Date.fromCalendarDate 1900 Mar 1)) ) ]
            , [ ( W.error "#N/A", Just (Err "#N/A") ), ( W.error "#DIV/0!", Just (Err "#DIV/0!") ), ( W.blank, Nothing ), ( W.float 1.0e21, Just (N 1.0e21) ) ]
            , [ ( W.formulaWithResult "SUM(B1:C1)" (W.float -38.85841), Just (F "SUM(B1:C1)" (N -38.85841)) )
              , ( W.formulaWithResult "=A1&\"!\"" (W.string "text!"), Just (F "A1&\"!\"" (S "text!")) )
              , ( W.formulaWithResult "A2" (W.bool True), Just (F "A2" (B True)) )
              , ( W.formula "XLOOKUP(-42,B1:C1,B1:C1)", Just (FNoValue "_xlfn.XLOOKUP(-42,B1:C1,B1:C1)") )
              ]
            ]
          )
        ]


text : Sample
text =
    makeSample "text"
        [ ( "Text"
          , [ [ ( W.string "  leading and trailing  ", Just (S "  leading and trailing  ") ) ]
            , [ ( W.string "line\nbreak", Just (S "line\nbreak") ) ]
            , [ ( W.string "tab\there", Just (S "tab\there") ) ]
            , [ ( W.string "<xml> & \"quotes\" 'apos'", Just (S "<xml> & \"quotes\" 'apos'") ) ]
            , [ ( W.string "control\u{0001}char", Just (S "control\u{0001}char") ) ]
            , [ ( W.string "literal _x0041_ escape", Just (S "literal _x0041_ escape") ) ]
            , [ ( W.string "emoji 👍🏽 and CJK 日本語 and RTL שלום", Just (S "emoji 👍🏽 and CJK 日本語 and RTL שלום") ) ]
            , [ ( W.string (String.repeat 32767 "x"), Just (S (String.repeat 32767 "x")) ) ]
            , [ ( W.string "", Just (S "") ) ]
            , [ ( W.string "repeated", Just (S "repeated") ), ( W.string "repeated", Just (S "repeated") ) ]
            ]
          )
        ]


multiSheet : Sample
multiSheet =
    { name = "multi-sheet"
    , sheets =
        [ ( "First", [ [ ( W.string "one", Just (S "one") ) ] ] )
        , ( "Bob's Sheet", [ [ ( W.string "two", Just (S "two") ) ] ] )
        , ( "Empty", [] )
        , ( "Résumé & <Notes>", [ [ ( W.formulaWithResult "'Bob''s Sheet'!A1" (W.string "two"), Just (F "'Bob''s Sheet'!A1" (S "two")) ) ] ] )
        ]
    , active = Just "Bob's Sheet"
    , compression = W.Deflate
    }


{-| 150,000 cells: more than 4 MB of sheet XML, so `Auto` stores the sheet
uncompressed.
-}
large : Sample
large =
    makeSample "large"
        [ ( "Large"
          , List.range 1 15000
                |> List.map
                    (\r ->
                        List.range 1 10
                            |> List.map
                                (\c ->
                                    if r == 15000 && c == 10 then
                                        ( W.float 1.5e5, Just (N 1.5e5) )

                                    else if modBy 2 c == 0 then
                                        ( W.string ("row " ++ String.fromInt r), Nothing )

                                    else
                                        ( W.int (r * c), Nothing )
                                )
                    )
          )
        ]



-- GOLDEN WORKBOOKS (same content as XlsxWriter's comparison tests)


goldens : List ( String, W.Workbook )
goldens =
    [ ( "golden-format01", format01 )
    , ( "golden-merge_range01", mergeRange01 )
    , ( "golden-set_column01", setColumn01 )
    , ( "golden-set_row01", setRow01 )
    , ( "golden-autofilter01", autofilter01 )
    , ( "golden-panes01", panes01 )
    , ( "golden-hyperlink31", hyperlink31 )
    , ( "golden-hyperlink03", hyperlink03 )
    , ( "golden-properties01", properties01 )
    , ( "showcase", showcase )
    ]


{-| Every styling and layout feature, for checking by eye in Excel, Numbers
and LibreOffice.
-}
showcase : W.Workbook
showcase =
    let
        title =
            Style.none |> Style.bold |> Style.fontSize 16 |> Style.horizontal Style.Center |> Style.vertical Style.Middle |> Style.fill (Style.rgb 221 235 247)

        boxed =
            Style.none |> Style.border Style.Thin Style.gray

        orders =
            [ { id = 1001, customer = "Grace Hopper", total = 1250.5, placed = Date.fromCalendarDate 2026 Nov 2, paid = True }
            , { id = 1002, customer = "Alan Turing", total = 89.99, placed = Date.fromCalendarDate 2026 Nov 7, paid = False }
            , { id = 1003, customer = "Katherine Johnson", total = 15000, placed = Date.fromCalendarDate 2026 Nov 13, paid = True }
            ]
    in
    W.workbook
        [ W.sheet "Styles"
            [ W.row ((W.string "Style showcase" |> W.withStyle title) :: List.repeat 3 (W.blank |> W.withStyle title))
                |> W.rowHeight 32
            , W.emptyRow
            , W.row [ W.string "Bold" |> W.withStyle (Style.none |> Style.bold), W.string "Italic" |> W.withStyle (Style.none |> Style.italic), W.string "Underline" |> W.withStyle (Style.none |> Style.underline), W.string "Strike" |> W.withStyle (Style.none |> Style.strikethrough) ]
            , W.row [ W.string "Red" |> W.withStyle (Style.none |> Style.fontColor Style.red), W.string "Arial 14" |> W.withStyle (Style.none |> Style.fontName "Arial" |> Style.fontSize 14), W.string "Yellow fill" |> W.withStyle (Style.none |> Style.fill Style.yellow), W.string "Boxed" |> W.withStyle boxed ]
            , W.row [ W.float 1234.5 |> W.withStyle (Style.none |> Style.numberFormat (Style.currency "$" 2)), W.float 0.256 |> W.withStyle (Style.none |> Style.numberFormat (Style.percent 1)), W.float 1234567.891 |> W.withStyle (Style.none |> Style.numberFormat (Style.thousands 2)), W.float 0.000123 |> W.withStyle (Style.none |> Style.numberFormat (Style.scientific 2)) ]
            , W.row [ W.string "Wrapped text that is long enough to need several lines" |> W.withStyle (Style.none |> Style.wrap), W.string "Right" |> W.withStyle (Style.none |> Style.horizontal Style.Right), W.string "Rotated" |> W.withStyle (Style.none |> Style.rotate 45), W.string "Indented" |> W.withStyle (Style.none |> Style.indent 2) ]
                |> W.rowHeight 60
            ]
            |> W.merge "A1:D1"
            |> W.columnWidths [ 22, 16, 16, 16 ]
        , Encode.table "Orders"
            [ Encode.int "Order" .id
            , Encode.string "Customer" .customer
            , Encode.float "Total" .total |> Encode.style (Style.none |> Style.numberFormat (Style.currency "$" 2))
            , Encode.date "Placed" .placed
            , Encode.bool "Paid" .paid
            ]
            orders
        ]


format01 : W.Workbook
format01 =
    W.workbook
        [ W.sheet "Sheet1" [ W.row [ W.string "Foo" ], W.row [ W.int 123 ] ]
        , W.sheet "Data Sheet" []
        , W.sheet "Sheet3"
            [ W.emptyRow
            , W.rowFrom 2 [ W.string "Foo" ]
            , W.rowFrom 2 [ W.string "Bar" |> W.withStyle (Style.none |> Style.bold) ]
            , W.rowFrom 3 [ W.int 234 ]
            ]
        ]


mergeRange01 : W.Workbook
mergeRange01 =
    let
        center =
            Style.none |> Style.horizontal Style.Center
    in
    W.workbook
        [ W.sheet "Sheet1"
            [ W.emptyRow
            , W.rowFrom 2 (List.map (W.withStyle center) [ W.string "Foo", W.blank, W.blank ])
            ]
            |> W.merge "B2:D2"
        ]


setColumn01 : W.Workbook
setColumn01 =
    let
        widths =
            [ ( 2, 0.17 ), ( 3, 0.25 ), ( 4, 0.33 ), ( 5, 0.42 ), ( 6, 0.5 ), ( 7, 0.58 ), ( 8, 0.67 ), ( 9, 0.75 ), ( 10, 0.83 ), ( 11, 0.92 ), ( 12, 1 ), ( 13, 1.14 ), ( 14, 1.29 ), ( 15, 1.43 ), ( 16, 1.57 ), ( 17, 1.71 ), ( 18, 1.86 ), ( 19, 2 ), ( 20, 2.14 ), ( 21, 2.29 ), ( 22, 2.43 ), ( 23, 2.57 ), ( 24, 2.71 ), ( 25, 2.86 ), ( 26, 3 ), ( 28, 8.57 ), ( 29, 8.71 ), ( 30, 8.86 ), ( 31, 9 ), ( 32, 9.14 ), ( 33, 9.29 ) ]
    in
    W.workbook
        [ List.foldl (\( c, w ) s -> W.columnWidth c w s)
            (W.sheet "Sheet1" [] |> W.columnWidth 1 0.08)
            widths
        ]


hyperlink31 : W.Workbook
hyperlink31 =
    W.workbook
        [ W.sheet "Sheet1"
            [ W.row [ W.string "Test" |> W.withStyle (Style.none |> Style.bold) ]
            , W.emptyRow
            , W.row [ W.hyperlink "http://www.python.org/" "http://www.python.org/" ]
            ]
        ]


hyperlink03 : W.Workbook
hyperlink03 =
    let
        link url =
            W.hyperlink url url

        perl =
            "http://www.perl.org/"

        cpan =
            "http://www.cpan.org/"
    in
    W.workbook
        [ W.sheet "Sheet1"
            [ W.row [ link perl ]
            , W.emptyRows 2
            , W.rowFrom 4 [ link perl ]
            , W.emptyRow
            , W.rowFrom 2 [ link cpan ]
            , W.emptyRow
            , W.row [ link perl ]
            , W.emptyRows 3
            , W.rowFrom 6 [ link cpan ]
            ]
        , W.sheet "Sheet2"
            [ W.emptyRow
            , W.rowFrom 3 [ link "http://www.google.com/" ]
            , W.emptyRows 2
            , W.rowFrom 3 [ link cpan ]
            , W.emptyRow
            , W.rowFrom 3 [ link perl ]
            ]
        ]


properties01 : W.Workbook
properties01 =
    W.workbook
        [ W.sheet "Sheet1"
            [ W.row [ W.string "Select 'Office Button -> Prepare -> Properties' to see the file properties." ] ]
            |> W.columnWidth 1 70
        ]
        |> W.withProperties
            [ W.title "This is an example spreadsheet"
            , W.subject "With document properties"
            , W.author "Someone"
            , W.manager "Dr. Heinz Doofenshmirtz"
            , W.company "of Wolves"
            , W.category "Example spreadsheets"
            , W.keywords "Sample, Example, Properties"
            , W.comments "Created with Perl and Excel::Writer::XLSX"
            , W.status "Quo"
            ]


setRow01 : W.Workbook
setRow01 =
    let
        tall h =
            W.emptyRow |> W.rowHeight h
    in
    W.workbook
        [ W.sheet "Sheet1"
            [ tall 0.75
            , tall 1.5
            , tall 2.25
            , tall 3
            , W.emptyRows 7
            , tall 9
            , tall 9.75
            , tall 10.5
            , tall 11.25
            , W.emptyRows 3
            , tall 14.25
            , W.emptyRow
            , tall 15.75
            , tall 16.5
            ]
        ]


autofilter01 : W.Workbook
autofilter01 =
    W.workbook
        [ W.sheet "Sheet1"
            [ W.row [ W.string "Region", W.string "Item", W.string "Volume", W.string "Month" ]
            , W.row [ W.string "East", W.string "Apple", W.float 9000, W.string "July" ]
            , W.row [ W.string "East", W.string "Apple", W.float 5000, W.string "July" ]
            , W.row [ W.string "South", W.string "Orange", W.float 9000, W.string "September" ]
            , W.row [ W.string "North", W.string "Apple", W.float 2000, W.string "November" ]
            , W.row [ W.string "West", W.string "Apple", W.float 9000, W.string "November" ]
            , W.row [ W.string "South", W.string "Pear", W.float 7000, W.string "October" ]
            , W.row [ W.string "North", W.string "Pear", W.float 9000, W.string "August" ]
            , W.row [ W.string "West", W.string "Orange", W.float 1000, W.string "December" ]
            , W.row [ W.string "West", W.string "Grape", W.float 1000, W.string "November" ]
            , W.row [ W.string "South", W.string "Pear", W.float 10000, W.string "April" ]
            , W.row [ W.string "West", W.string "Grape", W.float 6000, W.string "January" ]
            , W.row [ W.string "South", W.string "Orange", W.float 3000, W.string "May" ]
            , W.row [ W.string "North", W.string "Apple", W.float 3000, W.string "December" ]
            , W.row [ W.string "South", W.string "Apple", W.float 7000, W.string "February" ]
            , W.row [ W.string "West", W.string "Grape", W.float 1000, W.string "December" ]
            , W.row [ W.string "East", W.string "Grape", W.float 8000, W.string "February" ]
            , W.row [ W.string "South", W.string "Grape", W.float 10000, W.string "June" ]
            , W.row [ W.string "West", W.string "Pear", W.float 7000, W.string "December" ]
            , W.row [ W.string "South", W.string "Apple", W.float 2000, W.string "October" ]
            , W.row [ W.string "East", W.string "Grape", W.float 7000, W.string "December" ]
            , W.row [ W.string "North", W.string "Grape", W.float 6000, W.string "April" ]
            , W.row [ W.string "East", W.string "Pear", W.float 8000, W.string "February" ]
            , W.row [ W.string "North", W.string "Apple", W.float 7000, W.string "August" ]
            , W.row [ W.string "North", W.string "Orange", W.float 7000, W.string "July" ]
            , W.row [ W.string "North", W.string "Apple", W.float 6000, W.string "June" ]
            , W.row [ W.string "South", W.string "Grape", W.float 8000, W.string "September" ]
            , W.row [ W.string "West", W.string "Apple", W.float 3000, W.string "October" ]
            , W.row [ W.string "South", W.string "Orange", W.float 10000, W.string "November" ]
            , W.row [ W.string "West", W.string "Grape", W.float 4000, W.string "July" ]
            , W.row [ W.string "North", W.string "Orange", W.float 5000, W.string "August" ]
            , W.row [ W.string "East", W.string "Orange", W.float 1000, W.string "November" ]
            , W.row [ W.string "East", W.string "Orange", W.float 4000, W.string "October" ]
            , W.row [ W.string "North", W.string "Grape", W.float 5000, W.string "August" ]
            , W.row [ W.string "East", W.string "Apple", W.float 1000, W.string "December" ]
            , W.row [ W.string "South", W.string "Apple", W.float 10000, W.string "March" ]
            , W.row [ W.string "East", W.string "Grape", W.float 7000, W.string "October" ]
            , W.row [ W.string "West", W.string "Grape", W.float 1000, W.string "September" ]
            , W.row [ W.string "East", W.string "Grape", W.float 10000, W.string "October" ]
            , W.row [ W.string "South", W.string "Orange", W.float 8000, W.string "March" ]
            , W.row [ W.string "North", W.string "Apple", W.float 4000, W.string "July" ]
            , W.row [ W.string "South", W.string "Orange", W.float 5000, W.string "July" ]
            , W.row [ W.string "West", W.string "Apple", W.float 4000, W.string "June" ]
            , W.row [ W.string "East", W.string "Apple", W.float 5000, W.string "April" ]
            , W.row [ W.string "North", W.string "Pear", W.float 3000, W.string "August" ]
            , W.row [ W.string "East", W.string "Grape", W.float 9000, W.string "November" ]
            , W.row [ W.string "North", W.string "Orange", W.float 8000, W.string "October" ]
            , W.row [ W.string "East", W.string "Apple", W.float 10000, W.string "June" ]
            , W.row [ W.string "South", W.string "Pear", W.float 1000, W.string "December" ]
            , W.row [ W.string "North", W.string "Grape", W.float 10000, W.string "July" ]
            , W.row [ W.string "East", W.string "Grape", W.float 6000, W.string "February" ]
            ]
            |> W.autoFilter "A1:D51"
        ]


panes01 : W.Workbook
panes01 =
    let
        foo =
            [ W.row [ W.string "Foo" ] ]

        frozen n rows columns =
            W.sheet ("Sheet" ++ String.fromInt n) foo |> W.freeze { rows = rows, columns = columns }
    in
    W.workbook
        ([ frozen 1 1 0, frozen 2 2 0, frozen 3 0 1, frozen 4 0 2, frozen 5 1 1, frozen 6 3 6 ]
            ++ List.map (\n -> W.sheet ("Sheet" ++ String.fromInt n) foo) (List.range 7 13)
        )
