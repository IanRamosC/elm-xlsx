module Xlsx.Internal.StylesReader exposing (CellFormat, parse, general)

{-| Reads what the cell reader needs from `xl/styles.xml`: for each cell
style index (`s="3"`), its number format code and kind.

@docs CellFormat, parse, general

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Xlsx.Internal.NumFmt as NumFmt
import Xlsx.Internal.Xml.Tree as Tree


{-| -}
type alias CellFormat =
    { code : String
    , kind : NumFmt.Kind
    }


{-| The format of cells with no style, or with a style index that does not
exist.
-}
general : CellFormat
general =
    { code = "General", kind = NumFmt.Plain }


{-| The cell formats, indexed like `<cellXfs>`.
-}
parse : String -> Result String (Array CellFormat)
parse xml =
    Tree.parse xml
        |> Result.map
            (\root ->
                let
                    custom =
                        Tree.child "numFmts" root
                            |> Maybe.map (Tree.children "numFmt")
                            |> Maybe.withDefault []
                            |> List.filterMap
                                (\el ->
                                    Maybe.map2 Tuple.pair
                                        (Tree.attribute "numFmtId" el |> Maybe.andThen String.toInt)
                                        (Tree.attribute "formatCode" el)
                                )
                            |> Dict.fromList
                in
                Tree.child "cellXfs" root
                    |> Maybe.map (Tree.children "xf")
                    |> Maybe.withDefault []
                    |> List.map
                        (\xf ->
                            Tree.attribute "numFmtId" xf
                                |> Maybe.andThen String.toInt
                                |> Maybe.withDefault 0
                                |> formatFor custom
                        )
                    |> Array.fromList
            )


formatFor : Dict Int String -> Int -> CellFormat
formatFor custom id =
    case Dict.get id custom of
        Just code ->
            { code = code, kind = NumFmt.classify code }

        Nothing ->
            case NumFmt.builtinCode id of
                Just code ->
                    { code = code, kind = NumFmt.classify code }

                Nothing ->
                    if NumFmt.isDateFormatId id then
                        { code = NumFmt.defaultDate, kind = NumFmt.Date }

                    else
                        general
