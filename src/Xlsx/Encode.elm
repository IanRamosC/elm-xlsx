module Xlsx.Encode exposing
    ( table
    , Column, column, string, float, int, bool, date, dateTime
    , width, style
    , tableWith, TableOptions, defaultTableOptions
    )

{-| Turn a list of records into a sheet: one column per field, a header row on
top. This is the quickest way to an "Export to Excel" button.

    import Xlsx.Encode as Encode
    import Xlsx.Style as Style
    import Xlsx.Write as Write

    type alias Order =
        { id : Int, customer : String, total : Float, placed : Date }

    ordersSheet : List Order -> Write.Sheet
    ordersSheet orders =
        Encode.table "Orders"
            [ Encode.int "Order" .id
            , Encode.string "Customer" .customer
            , Encode.float "Total" .total
                |> Encode.style (Style.none |> Style.numberFormat (Style.currency "$" 2))
            , Encode.date "Placed" .placed
            ]
            orders

    bytes =
        Write.workbook [ ordersSheet orders ]
            |> Write.toBytes Write.defaultOptions

The sheet gets a bold header row that stays in place while scrolling, filter
buttons on the header, and column widths that fit the content. The result is
an ordinary `Write.Sheet`, so you can keep adjusting it with `Xlsx.Write`.

@docs table


# Columns

@docs Column, column, string, float, int, bool, date, dateTime
@docs width, style


# Options

@docs tableWith, TableOptions, defaultTableOptions

-}

import Date exposing (Date)
import Xlsx.Date exposing (DateTime)
import Xlsx.Internal.Model as Model exposing (Cell(..), Value(..))
import Xlsx.Ref as Ref
import Xlsx.Style as Style exposing (Style)
import Xlsx.Write as Write


{-| How to show one field of a record as a column.
-}
type Column a
    = Column
        { header : String
        , toCell : a -> Write.Cell
        , width : Maybe Float
        , style : Maybe Style
        }


{-| A column from any cell-making function. Use it for formulas, styled
cells or optional values:

    Encode.column "Email" (\user -> Maybe.map Write.string user.email |> Maybe.withDefault Write.blank)

-}
column : String -> (a -> Write.Cell) -> Column a
column header toCell =
    Column { header = header, toCell = toCell, width = Nothing, style = Nothing }


{-| -}
string : String -> (a -> String) -> Column a
string header get =
    column header (get >> Write.string)


{-| -}
float : String -> (a -> Float) -> Column a
float header get =
    column header (get >> Write.float)


{-| -}
int : String -> (a -> Int) -> Column a
int header get =
    column header (get >> Write.int)


{-| -}
bool : String -> (a -> Bool) -> Column a
bool header get =
    column header (get >> Write.bool)


{-| A date column, shown as `yyyy-mm-dd` unless you `style` it with another
number format.
-}
date : String -> (a -> Date) -> Column a
date header get =
    column header (get >> Write.date)


{-| -}
dateTime : String -> (a -> DateTime) -> Column a
dateTime header get =
    column header (get >> Write.dateTime)


{-| Set the column width (in Excel's units, about one digit each) instead of
fitting it to the content.
-}
width : Float -> Column a -> Column a
width w (Column c) =
    Column { c | width = Just w }


{-| Style the data cells of the column (not the header). A style the cell
function sets itself wins over this one, and calling `style` again layers the
new style on top.
-}
style : Style -> Column a -> Column a
style s (Column c) =
    Column
        { c
            | style =
                case c.style of
                    Just (Model.Style existing) ->
                        let
                            (Model.Style new) =
                                s
                        in
                        Just (Model.Style (Model.mergeStyles existing new))

                    Nothing ->
                        Just s
        }


{-| A sheet with a header row and one row per record, using
`defaultTableOptions`.
-}
table : String -> List (Column a) -> List a -> Write.Sheet
table =
    tableWith defaultTableOptions


{-| What `table` adds around the data.

  - `headerStyle`: the style of the header row.
  - `freezeHeader`: keep the header visible while scrolling.
  - `autoFilter`: filter buttons on the header.

-}
type alias TableOptions =
    { headerStyle : Style
    , freezeHeader : Bool
    , autoFilter : Bool
    }


{-| A bold header with a thin bottom border, frozen, with filter buttons.
-}
defaultTableOptions : TableOptions
defaultTableOptions =
    { headerStyle = Style.none |> Style.bold |> Style.borderBottom Style.Thin Style.black
    , freezeHeader = True
    , autoFilter = True
    }


{-| `table` with your own options.

    Encode.tableWith { defaultTableOptions | autoFilter = False } "Orders" columns orders

-}
tableWith : TableOptions -> String -> List (Column a) -> List a -> Write.Sheet
tableWith options name columns records =
    let
        specs =
            List.map (\(Column c) -> c) columns

        headerRow =
            Write.row (List.map (\c -> Write.string c.header |> Write.withStyle options.headerStyle) specs)

        dataCells =
            List.map (\record -> List.map (\c -> styleUnder c.style (c.toCell record)) specs) records

        lastRef =
            { column = max 1 (List.length specs), row = List.length records + 1 }

        withColumn ( index, c ) sheet =
            let
                fitted =
                    case c.width of
                        Just w ->
                            w

                        Nothing ->
                            fitWidth c.header (List.filterMap (List.drop index >> List.head) dataCells)
            in
            Write.columnWidth (index + 1) fitted sheet
    in
    Write.sheet name (headerRow :: List.map Write.row dataCells)
        |> (\sheet -> List.foldl withColumn sheet (List.indexedMap Tuple.pair specs))
        |> (if options.freezeHeader then
                Write.freeze { rows = 1, columns = 0 }

            else
                identity
           )
        |> (if options.autoFilter && not (List.isEmpty specs) then
                Write.autoFilter (Ref.rangeToString (Ref.range { column = 1, row = 1 } lastRef))

            else
                identity
           )


{-| Put a column's style underneath a cell's own style, so the cell wins.
-}
styleUnder : Maybe Style -> Write.Cell -> Write.Cell
styleUnder columnStyle (Cell c) =
    case columnStyle of
        Just (Model.Style base) ->
            Cell { c | style = Model.mergeStyles base c.style }

        Nothing ->
            Cell c


{-| A width that fits the header and the longest value, between 8 and 60
characters, plus room for the filter button.
-}
fitWidth : String -> List Write.Cell -> Float
fitWidth header cells =
    let
        longest =
            List.foldl (\cell acc -> max acc (displayLength cell)) (String.length header + 3) cells
    in
    toFloat (clamp 8 60 (longest + 2))


displayLength : Write.Cell -> Int
displayLength (Cell c) =
    valueLength c.value


valueLength : Value -> Int
valueLength value =
    case value of
        Text s ->
            String.lines s
                |> List.map String.length
                |> List.maximum
                |> Maybe.withDefault 0

        Number n ->
            String.length (String.fromFloat n)

        Boolean _ ->
            5

        DateValue _ ->
            10

        DateTimeValue _ ->
            19

        ErrorValue code ->
            String.length code

        Formula _ (Just cached) ->
            valueLength cached

        Formula _ Nothing ->
            8

        Blank ->
            0
