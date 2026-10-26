module Xlsx.Internal.Model exposing
    ( Workbook(..), WorkbookData
    , Sheet(..), SheetData, ColumnData
    , Row(..), RowData
    , Cell(..), CellData, Value(..)
    , Style(..), StyleData, Border, defaultStyle, mergeStyles
    , Problem
    )

{-| The data behind the opaque types of `Xlsx.Write`. Kept in an internal
module so several public modules can share it.

@docs Workbook, WorkbookData
@docs Sheet, SheetData, ColumnData
@docs Row, RowData
@docs Cell, CellData, Value
@docs Style, StyleData, Border, defaultStyle, mergeStyles
@docs Problem

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Xlsx.Date exposing (DateTime)


{-| -}
type Workbook
    = Workbook WorkbookData


{-| -}
type alias WorkbookData =
    { sheets : List Sheet
    , activeSheet : Maybe String
    }


{-| -}
type Sheet
    = Sheet SheetData


{-| -}
type alias SheetData =
    { name : String
    , rows : List Row
    , columns : Dict Int ColumnData
    , freeze : Maybe { rows : Int, columns : Int }
    , merges : List String
    , autoFilter : Maybe String
    }


{-| Settings for one column (1-based key in `SheetData.columns`).
-}
type alias ColumnData =
    { width : Maybe Float
    , hidden : Bool
    , style : Maybe StyleData
    }


{-| A row of cells, or a run of empty rows.
-}
type Row
    = Row RowData
    | EmptyRows Int (Maybe Float)


{-| `startColumn` is 1-based.
-}
type alias RowData =
    { startColumn : Int
    , cells : List Cell
    , height : Maybe Float
    }


{-| -}
type Cell
    = Cell CellData


{-| -}
type alias CellData =
    { value : Value
    , style : StyleData
    }


{-| What a cell holds. A formula's cached result is another value (never a
formula itself).
-}
type Value
    = Text String
    | Number Float
    | Boolean Bool
    | DateValue Date
    | DateTimeValue DateTime
    | ErrorValue String
    | Formula String (Maybe Value)
    | Blank


{-| Formatting applied to a cell. Every field is optional so styles can be
layered: a cell's own style wins over its column's style, field by field.
Colors are `AARRGGBB` hex strings; border styles and alignments use their
OOXML names (`"thin"`, `"center"`).
-}
type alias StyleData =
    { numberFormat : Maybe String
    , bold : Maybe Bool
    , italic : Maybe Bool
    , underline : Maybe Bool
    , strikethrough : Maybe Bool
    , fontSize : Maybe Float
    , fontColor : Maybe String
    , fontName : Maybe String
    , fill : Maybe String
    , borderLeft : Maybe Border
    , borderRight : Maybe Border
    , borderTop : Maybe Border
    , borderBottom : Maybe Border
    , horizontal : Maybe String
    , vertical : Maybe String
    , wrap : Maybe Bool
    , indent : Maybe Int
    , rotation : Maybe Int
    }


{-| The opaque style users build with `Xlsx.Style`.
-}
type Style
    = Style StyleData


{-| A border line: its OOXML style name and `AARRGGBB` color.
-}
type alias Border =
    { style : String
    , color : String
    }


{-| -}
defaultStyle : StyleData
defaultStyle =
    { numberFormat = Nothing
    , bold = Nothing
    , italic = Nothing
    , underline = Nothing
    , strikethrough = Nothing
    , fontSize = Nothing
    , fontColor = Nothing
    , fontName = Nothing
    , fill = Nothing
    , borderLeft = Nothing
    , borderRight = Nothing
    , borderTop = Nothing
    , borderBottom = Nothing
    , horizontal = Nothing
    , vertical = Nothing
    , wrap = Nothing
    , indent = Nothing
    , rotation = Nothing
    }


{-| Layer `top` over `base`: each field of `top` that is set wins.
-}
mergeStyles : StyleData -> StyleData -> StyleData
mergeStyles base top =
    { numberFormat = or top.numberFormat base.numberFormat
    , bold = or top.bold base.bold
    , italic = or top.italic base.italic
    , underline = or top.underline base.underline
    , strikethrough = or top.strikethrough base.strikethrough
    , fontSize = or top.fontSize base.fontSize
    , fontColor = or top.fontColor base.fontColor
    , fontName = or top.fontName base.fontName
    , fill = or top.fill base.fill
    , borderLeft = or top.borderLeft base.borderLeft
    , borderRight = or top.borderRight base.borderRight
    , borderTop = or top.borderTop base.borderTop
    , borderBottom = or top.borderBottom base.borderBottom
    , horizontal = or top.horizontal base.horizontal
    , vertical = or top.vertical base.vertical
    , wrap = or top.wrap base.wrap
    , indent = or top.indent base.indent
    , rotation = or top.rotation base.rotation
    }


or : Maybe a -> Maybe a -> Maybe a
or first second =
    case first of
        Just _ ->
            first

        Nothing ->
            second


{-| A reason a workbook cannot be written.
-}
type alias Problem =
    { sheet : Maybe String
    , cell : Maybe String
    , message : String
    }
