module Xlsx.Internal.Styles exposing
    ( Registry, empty, register, registerLink
    , toXml
    , hyperlinkStyle
    )

{-| Collect the distinct cell formats used in a workbook and write
`xl/styles.xml`.

Each distinct `Style` becomes one entry of `cellXfs`; a cell's `s` attribute is
the index of that entry. Index 0 is always the default format. Fonts, fills,
borders and custom number formats are deduplicated separately, as Excel does.

Hyperlink cells also use Excel's built-in "Hyperlink" cell style
(`cellStyleXfs` entry 1, `builtinId` 8), written only when a link is used.

@docs Registry, empty, register, registerLink
@docs toXml
@docs hyperlinkStyle

-}

import Dict exposing (Dict)
import Xlsx.Internal.Model as Model exposing (Border, StyleData)
import Xlsx.Internal.NumFmt as NumFmt
import Xlsx.Internal.Xml.Escape as Escape


{-| -}
type Registry
    = Registry
        { numFmts : Table String
        , fonts : Table Font
        , fills : Table String
        , borders : Table Borders
        , xfs : Table Xf
        , hyperlinkFont : Maybe Int
        }


{-| A deduplicating list: items in insertion order, each with an index.
-}
type alias Table a =
    { indexes : Dict String Int
    , reversed : List a
    , count : Int
    }


type alias Font =
    { bold : Bool
    , italic : Bool
    , underline : Bool
    , strikethrough : Bool
    , size : Float
    , color : String
    , name : String
    }


type alias Borders =
    { left : Maybe Border
    , right : Maybe Border
    , top : Maybe Border
    , bottom : Maybe Border
    }


type alias Xf =
    { numFmtId : Int
    , fontId : Int
    , fillId : Int
    , borderId : Int
    , horizontal : Maybe String
    , vertical : Maybe String
    , wrap : Bool
    , indent : Int
    , rotation : Int
    , hyperlink : Bool
    }


{-| Calibri 11, black: the basis of Excel's default column width.
-}
defaultFont : Font
defaultFont =
    { bold = False
    , italic = False
    , underline = False
    , strikethrough = False
    , size = 11
    , color = "FF000000"
    , name = "Calibri"
    }


noBorders : Borders
noBorders =
    { left = Nothing, right = Nothing, top = Nothing, bottom = Nothing }


defaultXf : Xf
defaultXf =
    { numFmtId = 0
    , fontId = 0
    , fillId = 0
    , borderId = 0
    , horizontal = Nothing
    , vertical = Nothing
    , wrap = False
    , indent = 0
    , rotation = 0
    , hyperlink = False
    }


{-| What Excel's Hyperlink style looks like: underlined, in the Office
hyperlink blue. Excel refers to the theme color; without a theme part the
color is spelled out.
-}
hyperlinkStyle : StyleData
hyperlinkStyle =
    { defaultStyle | underline = Just True, fontColor = Just hyperlinkColor }


defaultStyle : StyleData
defaultStyle =
    Model.defaultStyle


hyperlinkColor : String
hyperlinkColor =
    "FF0563C1"


hyperlinkFont : Font
hyperlinkFont =
    { defaultFont | underline = True, color = hyperlinkColor }


{-| A registry holding only the defaults: font 0, fills 0 (`none`) and 1
(`gray125`, reserved by Excel), border 0 and format 0.
-}
empty : Registry
empty =
    Registry
        { numFmts = emptyTable
        , fonts = singleton fontKey defaultFont
        , fills = reservedFills
        , borders = singleton bordersKey noBorders
        , xfs = singleton xfKey defaultXf
        , hyperlinkFont = Nothing
        }


{-| Fills 0 (`none`) and 1 (`gray125`) are reserved by Excel. Their keys
cannot clash with a color, so no style reuses them.
-}
reservedFills : Table String
reservedFills =
    { indexes = Dict.fromList [ ( "<none>", 0 ), ( "<gray125>", 1 ) ]
    , reversed = [ "gray125", "none" ]
    , count = 2
    }


emptyTable : Table a
emptyTable =
    { indexes = Dict.empty, reversed = [], count = 0 }


singleton : (a -> String) -> a -> Table a
singleton toKey item =
    { indexes = Dict.singleton (toKey item) 0, reversed = [ item ], count = 1 }


{-| Add an item if no item has the same key; give back its index.
-}
add : String -> a -> Table a -> ( Int, Table a )
add key item table =
    case Dict.get key table.indexes of
        Just index ->
            ( index, table )

        Nothing ->
            ( table.count
            , { indexes = Dict.insert key table.count table.indexes
              , reversed = item :: table.reversed
              , count = table.count + 1
              }
            )


{-| The `cellXfs` index for a style, adding it if it is new.
-}
register : StyleData -> Registry -> ( Int, Registry )
register =
    registerXf False


{-| The `cellXfs` index for a hyperlink cell. The style should already
include [`hyperlinkStyle`](#hyperlinkStyle) under the cell's own.
-}
registerLink : StyleData -> Registry -> ( Int, Registry )
registerLink style (Registry r) =
    case r.hyperlinkFont of
        Just _ ->
            registerXf True style (Registry r)

        Nothing ->
            let
                ( fontId, fonts ) =
                    add (fontKey hyperlinkFont) hyperlinkFont r.fonts
            in
            registerXf True style (Registry { r | fonts = fonts, hyperlinkFont = Just fontId })


registerXf : Bool -> StyleData -> Registry -> ( Int, Registry )
registerXf isLink style (Registry r) =
    if style == Model.defaultStyle && not isLink then
        ( 0, Registry r )

    else
        let
            ( numFmtId, numFmts ) =
                case style.numberFormat of
                    Nothing ->
                        ( 0, r.numFmts )

                    Just code ->
                        case NumFmt.builtinId code of
                            Just id ->
                                ( id, r.numFmts )

                            Nothing ->
                                add code code r.numFmts
                                    |> Tuple.mapFirst ((+) NumFmt.firstCustomId)

            font =
                { bold = Maybe.withDefault False style.bold
                , italic = Maybe.withDefault False style.italic
                , underline = Maybe.withDefault False style.underline
                , strikethrough = Maybe.withDefault False style.strikethrough
                , size = Maybe.withDefault defaultFont.size style.fontSize
                , color = Maybe.withDefault defaultFont.color style.fontColor
                , name = Maybe.withDefault defaultFont.name style.fontName
                }

            ( fontId, fonts ) =
                add (fontKey font) font r.fonts

            ( fillId, fills ) =
                case style.fill of
                    Nothing ->
                        ( 0, r.fills )

                    Just color ->
                        add color color r.fills

            borders =
                { left = style.borderLeft, right = style.borderRight, top = style.borderTop, bottom = style.borderBottom }

            ( borderId, borderTable ) =
                add (bordersKey borders) borders r.borders

            xf =
                { numFmtId = numFmtId
                , fontId = fontId
                , fillId = fillId
                , borderId = borderId
                , horizontal = style.horizontal
                , vertical = style.vertical
                , wrap = Maybe.withDefault False style.wrap
                , indent = Maybe.withDefault 0 style.indent
                , rotation = Maybe.withDefault 0 style.rotation
                , hyperlink = isLink
                }

            ( xfId, xfs ) =
                add (xfKey xf) xf r.xfs
        in
        ( xfId, Registry { r | numFmts = numFmts, fonts = fonts, fills = fills, borders = borderTable, xfs = xfs } )


fontKey : Font -> String
fontKey f =
    String.join "|"
        [ boolKey f.bold, boolKey f.italic, boolKey f.underline, boolKey f.strikethrough, String.fromFloat f.size, f.color, f.name ]


bordersKey : Borders -> String
bordersKey b =
    String.join "|" (List.map borderKey [ b.left, b.right, b.top, b.bottom ])


borderKey : Maybe Border -> String
borderKey border =
    case border of
        Just { style, color } ->
            style ++ "/" ++ color

        Nothing ->
            "-"


xfKey : Xf -> String
xfKey xf =
    String.join "|"
        [ String.fromInt xf.numFmtId
        , String.fromInt xf.fontId
        , String.fromInt xf.fillId
        , String.fromInt xf.borderId
        , Maybe.withDefault "-" xf.horizontal
        , Maybe.withDefault "-" xf.vertical
        , boolKey xf.wrap
        , String.fromInt xf.indent
        , String.fromInt xf.rotation
        , boolKey xf.hyperlink
        ]


boolKey : Bool -> String
boolKey b =
    if b then
        "1"

    else
        "0"



-- XML


{-| The `styleSheet` part, its elements in schema order.
-}
toXml : Registry -> String
toXml (Registry r) =
    String.concat
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
        , "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
        , numFmtsXml (List.reverse r.numFmts.reversed)
        , list "fonts" fontXml r.fonts
        , list "fills" fillXml r.fills
        , list "borders" bordersXml r.borders
        , case r.hyperlinkFont of
            Nothing ->
                "<cellStyleXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/></cellStyleXfs>"

            Just fontId ->
                -- the built-in Hyperlink style, exactly as Excel writes it
                "<cellStyleXfs count=\"2\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/>"
                    ++ "<xf numFmtId=\"0\" fontId=\""
                    ++ String.fromInt fontId
                    ++ "\" fillId=\"0\" borderId=\"0\" applyNumberFormat=\"0\" applyFill=\"0\" applyBorder=\"0\" applyAlignment=\"0\" applyProtection=\"0\">"
                    ++ "<alignment vertical=\"top\"/><protection locked=\"0\"/></xf></cellStyleXfs>"
        , list "cellXfs" (xfXml r.hyperlinkFont) r.xfs
        , case r.hyperlinkFont of
            Nothing ->
                "<cellStyles count=\"1\"><cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/></cellStyles>"

            Just _ ->
                "<cellStyles count=\"2\"><cellStyle name=\"Hyperlink\" xfId=\"1\" builtinId=\"8\"/><cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/></cellStyles>"
        , "<dxfs count=\"0\"/>"
        , "<tableStyles count=\"0\" defaultTableStyle=\"TableStyleMedium9\" defaultPivotStyle=\"PivotStyleLight16\"/>"
        , "</styleSheet>"
        ]


list : String -> (a -> String) -> Table a -> String
list tag toItem table =
    "<"
        ++ tag
        ++ " count=\""
        ++ String.fromInt table.count
        ++ "\">"
        ++ String.concat (List.map toItem (List.reverse table.reversed))
        ++ "</"
        ++ tag
        ++ ">"


numFmtsXml : List String -> String
numFmtsXml codes =
    if List.isEmpty codes then
        ""

    else
        "<numFmts count=\""
            ++ String.fromInt (List.length codes)
            ++ "\">"
            ++ String.concat
                (List.indexedMap
                    (\i code -> "<numFmt numFmtId=\"" ++ String.fromInt (NumFmt.firstCustomId + i) ++ "\" formatCode=\"" ++ Escape.attribute code ++ "\"/>")
                    codes
                )
            ++ "</numFmts>"


fontXml : Font -> String
fontXml f =
    String.concat
        [ "<font>"
        , flag f.bold "<b/>"
        , flag f.italic "<i/>"
        , flag f.strikethrough "<strike/>"
        , flag f.underline "<u/>"
        , "<sz val=\"" ++ String.fromFloat f.size ++ "\"/>"
        , "<color rgb=\"" ++ f.color ++ "\"/>"
        , "<name val=\"" ++ Escape.attribute f.name ++ "\"/>"
        , "<family val=\"2\"/>"
        , "</font>"
        ]


fillXml : String -> String
fillXml fill =
    case fill of
        "none" ->
            "<fill><patternFill patternType=\"none\"/></fill>"

        "gray125" ->
            "<fill><patternFill patternType=\"gray125\"/></fill>"

        color ->
            "<fill><patternFill patternType=\"solid\"><fgColor rgb=\"" ++ color ++ "\"/><bgColor indexed=\"64\"/></patternFill></fill>"


bordersXml : Borders -> String
bordersXml b =
    "<border>"
        ++ side "left" b.left
        ++ side "right" b.right
        ++ side "top" b.top
        ++ side "bottom" b.bottom
        ++ "<diagonal/></border>"


side : String -> Maybe Border -> String
side tag border =
    case border of
        Nothing ->
            "<" ++ tag ++ "/>"

        Just { style, color } ->
            "<" ++ tag ++ " style=\"" ++ style ++ "\"><color rgb=\"" ++ color ++ "\"/></" ++ tag ++ ">"


{-| A hyperlink cell whose font is the Hyperlink style's own inherits it, so
Excel does not mark the font as applied.
-}
xfXml : Maybe Int -> Xf -> String
xfXml linkFont xf =
    let
        alignmentAttrs =
            List.filterMap identity
                [ Maybe.map (attr "horizontal") xf.horizontal
                , Maybe.map (attr "vertical") xf.vertical
                , when (xf.rotation /= 0) (attr "textRotation" (String.fromInt xf.rotation))
                , when xf.wrap (attr "wrapText" "1")
                , when (xf.indent /= 0) (attr "indent" (String.fromInt xf.indent))
                ]

        applies =
            String.concat
                [ flag (xf.numFmtId /= 0) (attr "applyNumberFormat" "1")
                , flag (xf.fontId /= 0 && not (xf.hyperlink && Just xf.fontId == linkFont)) (attr "applyFont" "1")
                , flag (xf.fillId /= 0) (attr "applyFill" "1")
                , flag (xf.borderId /= 0) (attr "applyBorder" "1")
                , flag (not (List.isEmpty alignmentAttrs) || xf.hyperlink) (attr "applyAlignment" "1")
                , flag xf.hyperlink (attr "applyProtection" "1")
                ]

        open =
            "<xf numFmtId=\""
                ++ String.fromInt xf.numFmtId
                ++ "\" fontId=\""
                ++ String.fromInt xf.fontId
                ++ "\" fillId=\""
                ++ String.fromInt xf.fillId
                ++ "\" borderId=\""
                ++ String.fromInt xf.borderId
                ++ (if xf.hyperlink then
                        "\" xfId=\"1\""

                    else
                        "\" xfId=\"0\""
                   )
                ++ applies
    in
    if List.isEmpty alignmentAttrs then
        open ++ "/>"

    else
        open ++ "><alignment" ++ String.concat alignmentAttrs ++ "/></xf>"


attr : String -> String -> String
attr name value =
    " " ++ name ++ "=\"" ++ value ++ "\""


flag : Bool -> String -> String
flag on s =
    if on then
        s

    else
        ""


when : Bool -> a -> Maybe a
when on x =
    if on then
        Just x

    else
        Nothing
