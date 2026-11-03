port module Dump exposing (main)

{-| Read workbooks with Xlsx.Read and dump every cell as JSON, for
`scripts/oracle/compare.py` to check against python-calamine.
-}

import Base64
import Date
import Json.Decode as D
import Json.Encode as E
import Xlsx.Read as Read exposing (CellValue(..))
import Xlsx.Ref as Ref


port files : (D.Value -> msg) -> Sub msg


port dump : E.Value -> Cmd msg


main : Program () () D.Value
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \value m -> ( m, dump (dumpAll value) )
        , subscriptions = \_ -> files identity
        }


dumpAll : D.Value -> E.Value
dumpAll value =
    case D.decodeValue (D.list (D.map2 Tuple.pair (D.field "name" D.string) (D.field "base64" D.string))) value of
        Ok list ->
            E.list (\( name, b64 ) -> E.object [ ( "name", E.string name ), ( "result", dumpFile b64 ) ]) list

        Err e ->
            E.string (D.errorToString e)


dumpFile : String -> E.Value
dumpFile b64 =
    case Base64.toBytes b64 of
        Nothing ->
            E.object [ ( "error", E.string "bad base64" ) ]

        Just bytes ->
            case Read.read bytes of
                Err e ->
                    E.object [ ( "error", E.string (Read.errorToString e) ) ]

                Ok wb ->
                    E.object
                        [ ( "sheets"
                          , E.list
                                (\info ->
                                    E.object
                                        [ ( "name", E.string info.name )
                                        , ( "visibility", E.string (visibility info.visibility) )
                                        , ( "content"
                                          , case Read.sheet info.name wb of
                                                Ok sheet ->
                                                    E.object
                                                        [ ( "dimensions", sheet |> Read.dimensions |> Maybe.map (Ref.rangeToString >> E.string) |> Maybe.withDefault E.null )
                                                        , ( "merges", E.list (Ref.rangeToString >> E.string) (Read.merges sheet) )
                                                        , ( "cells", E.list cell (Read.cells sheet) )
                                                        ]

                                                Err e ->
                                                    E.object [ ( "error", E.string (Read.errorToString e) ) ]
                                          )
                                        ]
                                )
                                (Read.sheets wb)
                          )
                        ]


visibility : Read.Visibility -> String
visibility v =
    case v of
        Read.Visible ->
            "visible"

        Read.Hidden ->
            "hidden"

        Read.VeryHidden ->
            "veryHidden"


cell : ( Ref.Ref, Read.Cell ) -> E.Value
cell ( ref, c ) =
    let
        ( kind, v ) =
            case c.value of
                Text s ->
                    ( "text", E.string s )

                Number n ->
                    ( "number", E.float n )

                Boolean b ->
                    ( "bool", E.bool b )

                DateValue d ->
                    ( "date", E.string (Date.toIsoString d) )

                DateTimeValue dt ->
                    ( "datetime"
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

                ErrorValue e ->
                    ( "error", E.string e )

                Empty ->
                    ( "empty", E.null )
    in
    E.object
        [ ( "ref", E.string (Ref.toString ref) )
        , ( "type", E.string kind )
        , ( "value", v )
        , ( "formula", c.formula |> Maybe.map E.string |> Maybe.withDefault E.null )
        , ( "format", E.string c.numberFormat )
        ]


pad : Int -> String
pad n =
    String.padLeft 2 '0' (String.fromInt n)
