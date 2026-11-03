module SheetReaderTest exposing (suite)

import Array
import Expect
import Test exposing (Test, describe, test)
import Xlsx.Internal.NumFmt as NumFmt
import Xlsx.Internal.SheetReader as SheetReader exposing (RawValue(..))
import Xlsx.Internal.StylesReader as StylesReader


context : SheetReader.Context
context =
    { sharedStrings = Array.fromList [ "zero", "one" ]
    , formats = Array.fromList [ StylesReader.general, { code = "yyyy-mm-dd", kind = NumFmt.Date } ]
    }


values : String -> List ( ( Int, Int ), RawValue )
values xml =
    SheetReader.read context xml
        |> .cells
        |> List.map (\c -> ( ( c.row, c.column ), c.value ))


wrap : String -> String
wrap rows =
    "<?xml version=\"1.0\"?><worksheet xmlns=\"main\"><dimension ref=\"A1\"/><cols><col min=\"1\" max=\"1\" width=\"9\"/></cols><sheetData>" ++ rows ++ "</sheetData><mergeCells count=\"1\"><mergeCell ref=\"A1:B2\"/></mergeCells></worksheet>"


suite : Test
suite =
    describe "Xlsx.Internal.SheetReader"
        [ test "every cell type" <|
            \_ ->
                values
                    (wrap
                        ("<row r=\"1\"><c r=\"A1\" t=\"s\"><v>1</v></c><c r=\"B1\"><v>3.5</v></c><c r=\"C1\" t=\"b\"><v>1</v></c><c r=\"D1\" t=\"e\"><v>#N/A</v></c>"
                            ++ "<c r=\"E1\" t=\"str\"><f>A1&amp;\"!\"</f><v>one!</v></c><c r=\"F1\" t=\"inlineStr\"><is><t>in &lt;line&gt;</t></is></c><c r=\"G1\" t=\"d\"><v>2024-02-29T00:00:00</v></c>"
                            ++ "<c r=\"H1\" s=\"1\"/><c r=\"I1\"><v>1E-3</v></c></row>"
                        )
                    )
                    |> Expect.equal
                        [ ( ( 1, 1 ), RawText "one" )
                        , ( ( 1, 2 ), RawNumber 3.5 )
                        , ( ( 1, 3 ), RawBool True )
                        , ( ( 1, 4 ), RawError "#N/A" )
                        , ( ( 1, 5 ), RawText "one!" )
                        , ( ( 1, 6 ), RawText "in <line>" )
                        , ( ( 1, 7 ), RawIsoDate "2024-02-29T00:00:00" )
                        , ( ( 1, 9 ), RawNumber 0.001 )
                        ]
        , test "formulas: prefixes removed, shared children have none, uncached kept" <|
            \_ ->
                SheetReader.read context
                    (wrap "<row r=\"1\"><c r=\"A1\"><f t=\"shared\" ref=\"A1:A2\" si=\"0\">_xlfn.XLOOKUP(1,B:B,C:C)</f><v>2</v></c></row><row r=\"2\"><c r=\"A2\"><f t=\"shared\" si=\"0\"/><v>3</v></c><c r=\"B2\"><f>SUM(A1:A2)</f></c></row>")
                    |> .cells
                    |> List.map (\c -> ( c.formula, c.value ))
                    |> Expect.equal
                        [ ( Just "XLOOKUP(1,B:B,C:C)", RawNumber 2 )
                        , ( Nothing, RawNumber 3 )
                        , ( Just "SUM(A1:A2)", RawEmpty )
                        ]
        , test "missing r attributes follow the previous row and cell" <|
            \_ ->
                values (wrap "<row><c><v>1</v></c><c><v>2</v></c></row><row r=\"5\"><c r=\"C5\"><v>3</v></c><c><v>4</v></c></row><row><c><v>5</v></c></row>")
                    |> List.map Tuple.first
                    |> Expect.equal [ ( 1, 1 ), ( 1, 2 ), ( 5, 3 ), ( 5, 4 ), ( 6, 1 ) ]
        , test "x: prefix, single quotes and whitespace" <|
            \_ ->
                values "<x:worksheet xmlns:x=\"main\"><x:sheetData><x:row r='2'>\n  <x:c r='B2'\tt='s'>\n    <x:v>0</x:v>\n  </x:c>\n</x:row></x:sheetData></x:worksheet>"
                    |> Expect.equal [ ( ( 2, 2 ), RawText "zero" ) ]
        , test "extent, merges and format" <|
            \_ ->
                let
                    content =
                        SheetReader.read context (wrap "<row r=\"3\" spans=\"1:4\"><c r=\"D3\" s=\"1\"><v>45000</v></c><c r=\"Z3\" s=\"1\"/></row><row r=\"9\"><c r=\"A9\" s=\"1\"/></row>")
                in
                Expect.equal
                    ( ( content.lastRow, content.lastColumn ), content.merges, List.map (.format >> .kind) content.cells )
                    ( ( 3, 4 ), [ "A1:B2" ], [ NumFmt.Date ] )
        , test "empty sheetData" <|
            \_ ->
                SheetReader.read context "<worksheet><sheetData/></worksheet>"
                    |> (\c -> ( c.cells, c.lastRow, c.lastColumn ))
                    |> Expect.equal ( [], 0, 0 )
        , test "shared string index out of range is empty" <|
            \_ -> values (wrap "<row r=\"1\"><c r=\"A1\" t=\"s\"><v>99</v></c></row>") |> Expect.equal []
        ]
