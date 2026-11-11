module Xlsx.Style exposing
    ( Style, none
    , bold, italic, underline, strikethrough, fontSize, fontColor, fontName
    , fill
    , BorderStyle(..), border, borderTop, borderBottom, borderLeft, borderRight
    , Horizontal(..), Vertical(..), horizontal, vertical, wrap, indent, rotate
    , numberFormat
    , integer, decimal, thousands, percent, currency, scientific, text, isoDate, isoDateTime, time
    , Color, rgb, hex, black, white, gray, red, green, blue, yellow
    )

{-| How cells look: fonts, colors, borders, alignment and number formats.

Start from `none` and add what you need:

    import Xlsx.Style as Style

    header : Style.Style
    header =
        Style.none
            |> Style.bold
            |> Style.fill (Style.rgb 221 235 247)
            |> Style.borderBottom Style.Thin Style.black

    money : Style.Style
    money =
        Style.none
            |> Style.numberFormat (Style.currency "$" 2)

Apply a style to a cell with [`Xlsx.Write.withStyle`](Xlsx-Write#withStyle),
or to a whole column with [`Xlsx.Write.columnStyle`](Xlsx-Write#columnStyle).
Styles are layered: everything a cell's style sets wins over its column's
style, and anything it leaves unset comes from the column.

Excel can only hold about 64,000 distinct cell formats per workbook, but every
cell with the same style shares one, so reuse style values instead of building
a new one per cell when you can.


# Styles

@docs Style, none


# Font

@docs bold, italic, underline, strikethrough, fontSize, fontColor, fontName


# Background

@docs fill


# Borders

@docs BorderStyle, border, borderTop, borderBottom, borderLeft, borderRight


# Alignment

@docs Horizontal, Vertical, horizontal, vertical, wrap, indent, rotate


# Number formats

A number format decides how a number shows (`1234.5` as `$1,234.50`,
`0.25` as `25%`, `46339` as `2026-11-13`). It never changes the value
itself. Pass any Excel format code to `numberFormat`, or use the helpers below.

@docs numberFormat
@docs integer, decimal, thousands, percent, currency, scientific, text, isoDate, isoDateTime, time


# Colors

@docs Color, rgb, hex, black, white, gray, red, green, blue, yellow

-}

import Xlsx.Internal.Model as Model exposing (StyleData)


{-| A cell style. Build one by piping `none` through the functions below.
-}
type alias Style =
    Model.Style


{-| No formatting: Calibri 11, black, no fill, no borders, general number
format.
-}
none : Style
none =
    Model.Style Model.defaultStyle


update : (StyleData -> StyleData) -> Style -> Style
update f (Model.Style data) =
    Model.Style (f data)



-- FONT


{-| Bold text.
-}
bold : Style -> Style
bold =
    update (\s -> { s | bold = Just True })


{-| Italic text.
-}
italic : Style -> Style
italic =
    update (\s -> { s | italic = Just True })


{-| A single underline.
-}
underline : Style -> Style
underline =
    update (\s -> { s | underline = Just True })


{-| Text with a line through it.
-}
strikethrough : Style -> Style
strikethrough =
    update (\s -> { s | strikethrough = Just True })


{-| The font size in points. The default is 11.
-}
fontSize : Float -> Style -> Style
fontSize size =
    update (\s -> { s | fontSize = Just size })


{-| The text colour.

    Style.none |> Style.fontColor Style.red

-}
fontColor : Color -> Style -> Style
fontColor (Color color) =
    update (\s -> { s | fontColor = Just color })


{-| The font family, such as `"Arial"`. It must be installed on the computer
that opens the file. The default is Calibri.
-}
fontName : String -> Style -> Style
fontName name =
    update (\s -> { s | fontName = Just name })



-- FILL


{-| A solid background color.
-}
fill : Color -> Style -> Style
fill (Color color) =
    update (\s -> { s | fill = Just color })



-- BORDERS


{-| The line style of a border.
-}
type BorderStyle
    = Thin
    | Medium
    | Thick
    | Dashed
    | Dotted
    | Double
    | Hair


borderStyleName : BorderStyle -> String
borderStyleName style =
    case style of
        Thin ->
            "thin"

        Medium ->
            "medium"

        Thick ->
            "thick"

        Dashed ->
            "dashed"

        Dotted ->
            "dotted"

        Double ->
            "double"

        Hair ->
            "hair"


toBorder : BorderStyle -> Color -> Maybe Model.Border
toBorder style (Color color) =
    Just { style = borderStyleName style, color = color }


{-| The same border on all four sides.

    Style.none |> Style.border Style.Thin Style.gray

-}
border : BorderStyle -> Color -> Style -> Style
border style color =
    update
        (\s ->
            let
                b =
                    toBorder style color
            in
            { s | borderTop = b, borderBottom = b, borderLeft = b, borderRight = b }
        )


{-| A border above the cell.

    Style.none |> Style.borderTop Style.Thin Style.gray

-}
borderTop : BorderStyle -> Color -> Style -> Style
borderTop style color =
    update (\s -> { s | borderTop = toBorder style color })


{-| A border below the cell.
-}
borderBottom : BorderStyle -> Color -> Style -> Style
borderBottom style color =
    update (\s -> { s | borderBottom = toBorder style color })


{-| A border on the left of the cell.
-}
borderLeft : BorderStyle -> Color -> Style -> Style
borderLeft style color =
    update (\s -> { s | borderLeft = toBorder style color })


{-| A border on the right of the cell.
-}
borderRight : BorderStyle -> Color -> Style -> Style
borderRight style color =
    update (\s -> { s | borderRight = toBorder style color })



-- ALIGNMENT


{-| Horizontal alignment. Without one, Excel puts text on the left and
numbers on the right.
-}
type Horizontal
    = Left
    | Center
    | Right
    | Justify


{-| Vertical alignment. Excel's default is `Bottom`.
-}
type Vertical
    = Top
    | Middle
    | Bottom


{-| Align the content horizontally.

    Style.none |> Style.horizontal Style.Center

-}
horizontal : Horizontal -> Style -> Style
horizontal h =
    update
        (\s ->
            { s
                | horizontal =
                    Just
                        (case h of
                            Left ->
                                "left"

                            Center ->
                                "center"

                            Right ->
                                "right"

                            Justify ->
                                "justify"
                        )
            }
        )


{-| Align the content vertically.
-}
vertical : Vertical -> Style -> Style
vertical v =
    update
        (\s ->
            { s
                | vertical =
                    Just
                        (case v of
                            Top ->
                                "top"

                            Middle ->
                                "center"

                            Bottom ->
                                "bottom"
                        )
            }
        )


{-| Wrap long text onto several lines. Also needed for line breaks (`\n`) in
a cell to show.
-}
wrap : Style -> Style
wrap =
    update (\s -> { s | wrap = Just True })


{-| Indent the content by a number of levels (about 3 characters each),
from 0 to 250.
-}
indent : Int -> Style -> Style
indent n =
    update (\s -> { s | indent = Just (clamp 0 250 n) })


{-| Rotate text by an angle in degrees, from -90 (downward) to 90 (upward).
-}
rotate : Int -> Style -> Style
rotate degrees =
    let
        clamped =
            clamp -90 90 degrees

        -- OOXML stores downward angles as 91–180.
        stored =
            if clamped < 0 then
                90 - clamped

            else
                clamped
    in
    update (\s -> { s | rotation = Just stored })



-- NUMBER FORMATS


{-| Show numbers with an Excel format code.

    Style.none |> Style.numberFormat "#,##0.00 \"kg\""

-}
numberFormat : String -> Style -> Style
numberFormat code =
    update (\s -> { s | numberFormat = Just code })


{-| `0`: whole numbers.
-}
integer : String
integer =
    "0"


{-| A fixed number of decimal places: `decimal 2` is `0.00`.
-}
decimal : Int -> String
decimal places =
    "0" ++ decimals places


{-| Thousands separators and decimal places: `thousands 2` is `#,##0.00`.
-}
thousands : Int -> String
thousands places =
    "#,##0" ++ decimals places


{-| A percentage with decimal places: `percent 1` shows `0.256` as `25.6%`.
-}
percent : Int -> String
percent places =
    "0" ++ decimals places ++ "%"


{-| A currency symbol before the amount, with thousands separators and
decimal places. Negative amounts get a minus sign.

    currency "$" 2 == "\"$\"#,##0.00"

    currency "€" 0 == "\"€\"#,##0"

-}
currency : String -> Int -> String
currency symbol places =
    "\"" ++ String.replace "\"" "" symbol ++ "\"" ++ thousands places


{-| Scientific notation with decimal places: `scientific 2` is `0.00E+00`.
-}
scientific : Int -> String
scientific places =
    "0" ++ decimals places ++ "E+00"


{-| `@`: show the content as typed, as text.
-}
text : String
text =
    "@"


{-| `yyyy-mm-dd`, the default for date cells.
-}
isoDate : String
isoDate =
    "yyyy-mm-dd"


{-| `yyyy-mm-dd hh:mm:ss`, the default for date-time cells.
-}
isoDateTime : String
isoDateTime =
    "yyyy-mm-dd hh:mm:ss"


{-| `hh:mm:ss`: just the time of day.
-}
time : String
time =
    "hh:mm:ss"


decimals : Int -> String
decimals places =
    if places <= 0 then
        ""

    else
        "." ++ String.repeat places "0"



-- COLORS


{-| An opaque RGB color.
-}
type Color
    = Color String


{-| A color from red, green and blue components, each 0–255 (values outside
are clamped).

    Style.rgb 31 78 121

-}
rgb : Int -> Int -> Int -> Color
rgb r g b =
    Color ("FF" ++ hexByte r ++ hexByte g ++ hexByte b)


{-| A color from a CSS-style hex code, with or without `#`: `"#1F4E79"`,
`"1f4e79"` or the short form `"#fff"`.

    Style.hex "#1F4E79" == Just (Style.rgb 31 78 121)

    Style.hex "blue" == Nothing

-}
hex : String -> Maybe Color
hex input =
    let
        digits =
            String.toUpper
                (if String.startsWith "#" input then
                    String.dropLeft 1 input

                 else
                    input
                )

        expanded =
            if String.length digits == 3 then
                String.concat (List.map (\c -> String.fromList [ c, c ]) (String.toList digits))

            else
                digits
    in
    if String.length expanded == 6 && String.all Char.isHexDigit expanded then
        Just (Color ("FF" ++ expanded))

    else
        Nothing


hexByte : Int -> String
hexByte n =
    let
        v =
            clamp 0 255 n

        digit d =
            String.slice d (d + 1) "0123456789ABCDEF"
    in
    digit (v // 16) ++ digit (modBy 16 v)


{-| `#000000`
-}
black : Color
black =
    rgb 0 0 0


{-| `#FFFFFF`
-}
white : Color
white =
    rgb 255 255 255


{-| A medium gray, `#BFBFBF`, good for light borders.
-}
gray : Color
gray =
    rgb 191 191 191


{-| `#FF0000`
-}
red : Color
red =
    rgb 255 0 0


{-| Excel's green, `#00B050`.
-}
green : Color
green =
    rgb 0 176 80


{-| Excel's blue, `#0070C0`.
-}
blue : Color
blue =
    rgb 0 112 192


{-| `#FFFF00`
-}
yellow : Color
yellow =
    rgb 255 255 0
