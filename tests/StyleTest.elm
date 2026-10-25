module StyleTest exposing (suite)

import Date
import Expect
import Helpers exposing (contains, containsAll)
import Test exposing (Test, describe, test)
import Time exposing (Month(..))
import Xlsx.Style as Style
import Xlsx.Write as W


styles : String
styles =
    "xl/styles.xml"


sheet1 : String
sheet1 =
    "xl/worksheets/sheet1.xml"


one : List W.Cell -> W.Workbook
one cs =
    W.workbook [ W.sheet "S" [ W.row cs ] ]


styled : Style.Style -> W.Workbook
styled style =
    one [ W.int 1 |> W.withStyle style ]


suite : Test
suite =
    describe "Xlsx.Style"
        [ describe "fonts"
            [ test "bold italic underline strike size color name" <|
                \_ ->
                    styled
                        (Style.none
                            |> Style.bold
                            |> Style.italic
                            |> Style.underline
                            |> Style.strikethrough
                            |> Style.fontSize 14
                            |> Style.fontColor (Style.rgb 255 0 0)
                            |> Style.fontName "Arial"
                        )
                        |> containsAll styles
                            [ "<fonts count=\"2\"><font><sz val=\"11\"/><color rgb=\"FF000000\"/><name val=\"Calibri\"/><family val=\"2\"/></font><font><b/><i/><strike/><u/><sz val=\"14\"/><color rgb=\"FFFF0000\"/><name val=\"Arial\"/><family val=\"2\"/></font></fonts>"
                            , "<xf numFmtId=\"0\" fontId=\"1\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyFont=\"1\"/>"
                            ]
            , test "equal fonts are shared" <|
                \_ ->
                    one
                        [ W.int 1 |> W.withStyle (Style.none |> Style.bold)
                        , W.int 2 |> W.withStyle (Style.none |> Style.bold |> Style.fill Style.yellow)
                        ]
                        |> containsAll styles [ "<fonts count=\"2\">", "<cellXfs count=\"3\">" ]
            ]
        , describe "fills"
            [ test "solid fill after the two reserved fills" <|
                \_ ->
                    styled (Style.none |> Style.fill (Style.rgb 221 235 247))
                        |> containsAll styles
                            [ "<fills count=\"3\"><fill><patternFill patternType=\"none\"/></fill><fill><patternFill patternType=\"gray125\"/></fill><fill><patternFill patternType=\"solid\"><fgColor rgb=\"FFDDEBF7\"/><bgColor indexed=\"64\"/></patternFill></fill></fills>"
                            , "fillId=\"2\""
                            , "applyFill=\"1\""
                            ]
            ]
        , describe "borders"
            [ test "all sides" <|
                \_ ->
                    styled (Style.none |> Style.border Style.Thin Style.black)
                        |> contains styles "<borders count=\"2\"><border><left/><right/><top/><bottom/><diagonal/></border><border><left style=\"thin\"><color rgb=\"FF000000\"/></left><right style=\"thin\"><color rgb=\"FF000000\"/></right><top style=\"thin\"><color rgb=\"FF000000\"/></top><bottom style=\"thin\"><color rgb=\"FF000000\"/></bottom><diagonal/></border></borders>"
            , test "one side" <|
                \_ ->
                    styled (Style.none |> Style.borderBottom Style.Double Style.blue)
                        |> contains styles "<border><left/><right/><top/><bottom style=\"double\"><color rgb=\"FF0070C0\"/></bottom><diagonal/></border>"
            ]
        , describe "alignment"
            [ test "horizontal vertical wrap indent" <|
                \_ ->
                    styled (Style.none |> Style.horizontal Style.Center |> Style.vertical Style.Middle |> Style.wrap |> Style.indent 2)
                        |> contains styles "<xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyAlignment=\"1\"><alignment horizontal=\"center\" vertical=\"center\" wrapText=\"1\" indent=\"2\"/></xf>"
            , test "downward rotation is stored as 90 + angle" <|
                \_ ->
                    styled (Style.none |> Style.rotate -45)
                        |> contains styles "<alignment textRotation=\"135\"/>"
            ]
        , describe "number formats"
            [ test "built-in formats reuse their id" <|
                \_ ->
                    styled (Style.none |> Style.numberFormat (Style.thousands 2))
                        |> Expect.all
                            [ contains styles "<xf numFmtId=\"4\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyNumberFormat=\"1\"/>"
                            , \wb ->
                                case Helpers.writeOk wb |> Result.toMaybe |> Maybe.andThen (Helpers.part styles) of
                                    Just xml ->
                                        Expect.equal False (String.contains "<numFmts" xml)

                                    Nothing ->
                                        Expect.fail "no styles"
                            ]
            , test "custom formats from 164, escaped" <|
                \_ ->
                    one
                        [ W.int 1 |> W.withStyle (Style.none |> Style.numberFormat (Style.currency "$" 2))
                        , W.int 1 |> W.withStyle (Style.none |> Style.numberFormat "0.0 \"kg\"")
                        ]
                        |> contains styles "<numFmts count=\"2\"><numFmt numFmtId=\"164\" formatCode=\"&quot;$&quot;#,##0.00\"/><numFmt numFmtId=\"165\" formatCode=\"0.0 &quot;kg&quot;\"/></numFmts>"
            , test "helpers" <|
                \_ ->
                    Expect.equal
                        [ "0", "0.00", "#,##0", "#,##0.0", "0%", "0.00%", "\"€\"#,##0", "0.00E+00", "@" ]
                        [ Style.integer, Style.decimal 2, Style.thousands 0, Style.thousands 1, Style.percent 0, Style.percent 2, Style.currency "€" 0, Style.scientific 2, Style.text ]
            ]
        , describe "layering"
            [ test "later styles win, unset settings are kept" <|
                \_ ->
                    one
                        [ W.int 1
                            |> W.withStyle (Style.none |> Style.bold |> Style.fontColor Style.red)
                            |> W.withStyle (Style.none |> Style.fontColor Style.blue)
                        ]
                        |> contains styles "<font><b/><sz val=\"11\"/><color rgb=\"FF0070C0\"/>"
            , test "a date keeps its default format when only the font is styled" <|
                \_ ->
                    one [ W.date (Date.fromCalendarDate 2026 Nov 13) |> W.withStyle (Style.none |> Style.bold) ]
                        |> Expect.all
                            [ contains sheet1 "<c r=\"A1\" s=\"1\"><v>46339</v></c>"
                            , contains styles "<xf numFmtId=\"164\" fontId=\"1\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyNumberFormat=\"1\" applyFont=\"1\"/>"
                            ]
            , test "the same style on many cells shares one format" <|
                \_ ->
                    one (List.repeat 5 (W.int 1 |> W.withStyle (Style.none |> Style.bold)))
                        |> contains styles "<cellXfs count=\"2\">"
            , test "a styled blank cell is written" <|
                \_ ->
                    one [ W.blank |> W.withStyle (Style.none |> Style.fill Style.yellow) ]
                        |> contains sheet1 "<c r=\"A1\" s=\"1\"/>"
            ]
        , describe "colors"
            [ test "hex with and without #" <|
                \_ -> Expect.equal [ Just (Style.rgb 31 78 121), Just (Style.rgb 31 78 121) ] [ Style.hex "#1F4E79", Style.hex "1f4e79" ]
            , test "short hex" <| \_ -> Expect.equal (Just Style.white) (Style.hex "#fff")
            , test "invalid hex" <| \_ -> Expect.equal [ Nothing, Nothing ] [ Style.hex "blue", Style.hex "#12345" ]
            , test "rgb clamps" <| \_ -> Expect.equal (Style.rgb 255 0 0) (Style.rgb 300 -5 0)
            ]
        ]
