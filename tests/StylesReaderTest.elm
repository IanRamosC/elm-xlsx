module StylesReaderTest exposing (suite)

import Array
import Expect
import Test exposing (Test, describe, test)
import Xlsx.Internal.NumFmt as NumFmt
import Xlsx.Internal.StylesReader as StylesReader


suite : Test
suite =
    describe "Xlsx.Internal.StylesReader"
        [ test "built-in, custom, East Asian and missing formats" <|
            \_ ->
                StylesReader.parse
                    ("<styleSheet xmlns=\"main\"><numFmts count=\"2\"><numFmt numFmtId=\"164\" formatCode=\"yyyy-mm-dd\"/><numFmt numFmtId=\"165\" formatCode=\"&quot;$&quot;#,##0.00\"/></numFmts>"
                        ++ "<cellXfs count=\"6\"><xf numFmtId=\"0\"/><xf numFmtId=\"164\" applyNumberFormat=\"1\"/><xf numFmtId=\"165\"/><xf numFmtId=\"22\"/><xf numFmtId=\"31\"/><xf fontId=\"1\"/></cellXfs></styleSheet>"
                    )
                    |> Result.map Array.toList
                    |> Expect.equal
                        (Ok
                            [ StylesReader.general
                            , { code = "yyyy-mm-dd", kind = NumFmt.Date }
                            , { code = "\"$\"#,##0.00", kind = NumFmt.Plain }
                            , { code = "m/d/yy h:mm", kind = NumFmt.DateTime }
                            , { code = "yyyy-mm-dd", kind = NumFmt.Date }
                            , StylesReader.general
                            ]
                        )
        , test "prefixed and without numFmts" <|
            \_ ->
                StylesReader.parse "<x:styleSheet xmlns:x=\"main\"><x:cellXfs><x:xf numFmtId=\"14\"/></x:cellXfs></x:styleSheet>"
                    |> Result.map Array.toList
                    |> Expect.equal (Ok [ { code = "m/d/yy", kind = NumFmt.Date } ])
        , test "200k cell formats parse in linear time, without stack overflow" <|
            \_ ->
                StylesReader.parse ("<styleSheet><cellXfs>" ++ String.repeat 200000 "<xf numFmtId=\"14\" fontId=\"0\"/>" ++ "</cellXfs></styleSheet>")
                    |> Result.map Array.length
                    |> Expect.equal (Ok 200000)
        ]
