module Main exposing (main)

{-| Open an `.xlsx` file, look at its sheets, decode a contact list into
records, and export the records again.

Run it with `elm reactor` from this folder, or build it with
`elm make src/Main.elm --output=index.html`.

-}

import Browser
import Bytes exposing (Bytes)
import Contacts exposing (Contact)
import Date exposing (Date)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, button, div, h1, h2, li, p, pre, table, td, text, th, tr, ul)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import Task
import Time
import Xlsx.Decode as Decode
import Xlsx.Read as Read exposing (CellValue(..))
import Xlsx.Write as Write


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { fileName : Maybe String
    , workbook : Maybe Read.Workbook
    , selected : Maybe String
    , error : Maybe String
    }


initialModel : Model
initialModel =
    { fileName = Nothing, workbook = Nothing, selected = Nothing, error = Nothing }


type Msg
    = DownloadSample
    | PickFile
    | FilePicked File
    | FileLoaded String Bytes
    | SelectSheet String
    | Export (List Contact)
    | ExportAt (List Contact) Time.Posix


mime : String
mime =
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownloadSample ->
            case Write.toBytes Write.defaultOptions Contacts.sample of
                Ok bytes ->
                    ( model, File.Download.bytes "contacts.xlsx" mime bytes )

                Err problems ->
                    ( { model | error = Just (String.join "\n" (List.map Write.problemToString problems)) }, Cmd.none )

        PickFile ->
            ( model, File.Select.file [ mime, ".xlsx" ] FilePicked )

        FilePicked file ->
            ( model, Task.perform (FileLoaded (File.name file)) (File.toBytes file) )

        FileLoaded fileName bytes ->
            case Read.read bytes of
                Ok workbook ->
                    ( { fileName = Just fileName
                      , workbook = Just workbook
                      , selected = List.head (Read.sheetNames workbook)
                      , error = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { initialModel | fileName = Just fileName, error = Just (Read.errorToString error) }, Cmd.none )

        SelectSheet sheetName ->
            ( { model | selected = Just sheetName }, Cmd.none )

        Export contacts ->
            ( model, Task.perform (ExportAt contacts) Time.now )

        ExportAt contacts now ->
            case Write.toBytes { created = now, compression = Write.Auto } (Contacts.export contacts) of
                Ok bytes ->
                    ( model, File.Download.bytes "contacts-export.xlsx" mime bytes )

                Err problems ->
                    ( { model | error = Just (String.join "\n" (List.map Write.problemToString problems)) }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "font-family" "system-ui, sans-serif", style "max-width" "960px", style "margin" "2rem auto", style "padding" "0 16px" ]
        [ h1 [] [ text "elm-xlsx example" ]
        , p []
            [ button [ onClick DownloadSample ] [ text "Download a sample file" ]
            , text " "
            , button [ onClick PickFile ] [ text "Open an .xlsx file" ]
            ]
        , case model.error of
            Just error ->
                pre [ style "color" "#b00020", style "white-space" "pre-wrap" ] [ text error ]

            Nothing ->
                text ""
        , case ( model.workbook, model.fileName ) of
            ( Just workbook, Just fileName ) ->
                viewWorkbook fileName workbook model.selected

            _ ->
                p [] [ text "Download the sample, then open it. Or open any .xlsx file you have." ]
        ]


viewWorkbook : String -> Read.Workbook -> Maybe String -> Html Msg
viewWorkbook fileName workbook selected =
    div []
        [ h2 [] [ text fileName ]
        , p [] (List.map (viewSheetButton selected) (Read.sheets workbook))
        , case selected of
            Just sheetName ->
                case Read.sheet sheetName workbook of
                    Ok sheet ->
                        div [] [ viewDecoded sheet, viewSheet sheet ]

                    Err error ->
                        pre [ style "color" "#b00020" ] [ text (Read.errorToString error) ]

            Nothing ->
                p [] [ text "This workbook has no worksheets." ]
        ]


viewSheetButton : Maybe String -> Read.SheetInfo -> Html Msg
viewSheetButton selected info =
    let
        readable =
            info.kind == Read.Worksheet

        label =
            info.name
                ++ (case info.visibility of
                        Read.Visible ->
                            ""

                        _ ->
                            " (hidden)"
                   )
    in
    button
        [ onClick (SelectSheet info.name)
        , disabled (not readable)
        , style "font-weight"
            (if selected == Just info.name then
                "bold"

             else
                "normal"
            )
        , style "margin-right" "4px"
        ]
        [ text label ]


{-| Try the contact decoder on the sheet.
-}
viewDecoded : Read.Sheet -> Html Msg
viewDecoded sheet =
    case Decode.decodeSheet Decode.FieldNamesFromFirstRow Contacts.decoder sheet of
        Ok contacts ->
            div []
                [ p [] [ text ("Decoded " ++ String.fromInt (List.length contacts) ++ " contacts. ") ]
                , button [ onClick (Export contacts) ] [ text "Export them as a new file" ]
                ]

        Err error ->
            div []
                [ p [] [ text "This sheet is not a contact list (columns Name, Email, Age and Joined):" ]
                , pre [ style "white-space" "pre-wrap", style "background" "#f6f6f6", style "padding" "8px" ] [ text (Decode.errorToString error) ]
                ]


viewSheet : Read.Sheet -> Html Msg
viewSheet sheet =
    let
        rows =
            Read.rows sheet |> List.take 200

        width =
            List.map List.length rows |> List.maximum |> Maybe.withDefault 0

        cellStyle =
            [ style "border" "1px solid #ddd", style "padding" "2px 6px", style "text-align" "left" ]
    in
    div [ style "overflow-x" "auto" ]
        [ table [ style "border-collapse" "collapse", style "font-size" "14px" ]
            (tr [] (th cellStyle [] :: List.map (\c -> th cellStyle [ text (columnLetter c) ]) (List.range 1 width))
                :: List.indexedMap
                    (\i cells ->
                        tr []
                            (th cellStyle [ text (String.fromInt (i + 1)) ]
                                :: List.map (\v -> td cellStyle [ text (showValue v) ]) cells
                            )
                    )
                    rows
            )
        , if List.length (Read.rows sheet) > 200 then
            p [] [ text "Showing the first 200 rows." ]

          else
            text ""
        ]


columnLetter : Int -> String
columnLetter n =
    if n <= 0 then
        ""

    else
        columnLetter ((n - 1) // 26) ++ String.fromChar (Char.fromCode (65 + modBy 26 (n - 1)))


showValue : CellValue -> String
showValue value =
    case value of
        Text s ->
            s

        Number n ->
            String.fromFloat n

        Boolean b ->
            if b then
                "TRUE"

            else
                "FALSE"

        DateValue d ->
            Date.toIsoString d

        DateTimeValue dt ->
            Date.toIsoString dt.date ++ " " ++ String.join ":" (List.map (String.fromInt >> String.padLeft 2 '0') [ dt.hour, dt.minute, dt.second ])

        ErrorValue e ->
            e

        Empty ->
            ""
