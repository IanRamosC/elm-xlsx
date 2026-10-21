module Xlsx.Internal.Model exposing
    ( Workbook(..), WorkbookData
    , Sheet(..), SheetData
    , Row(..), RowData
    , Cell(..), CellData, Value(..)
    , Style, defaultStyle
    , Problem
    )

{-| The data behind the opaque types of `Xlsx.Write`. Kept in an internal
module so several public modules can share it.

@docs Workbook, WorkbookData
@docs Sheet, SheetData
@docs Row, RowData
@docs Cell, CellData, Value
@docs Style, defaultStyle
@docs Problem

-}

import Date exposing (Date)
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
    }


{-| A row of cells, or a run of empty rows.
-}
type Row
    = Row RowData
    | EmptyRows Int


{-| `startColumn` is 1-based.
-}
type alias RowData =
    { startColumn : Int
    , cells : List Cell
    }


{-| -}
type Cell
    = Cell CellData


{-| -}
type alias CellData =
    { value : Value
    , style : Style
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


{-| Formatting applied to a cell.
-}
type alias Style =
    { numberFormat : Maybe String
    }


{-| -}
defaultStyle : Style
defaultStyle =
    { numberFormat = Nothing
    }


{-| A reason a workbook cannot be written.
-}
type alias Problem =
    { sheet : Maybe String
    , cell : Maybe String
    , message : String
    }
