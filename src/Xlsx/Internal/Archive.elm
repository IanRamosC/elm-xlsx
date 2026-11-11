module Xlsx.Internal.Archive exposing
    ( Archive, ArchiveError(..), Limits, open
    , PartError(..), readText, has, paths
    )

{-| The ZIP container of an `.xlsx` file, opened safely.

  - The file type is checked from its first bytes: ZIP files start with
    `PK\u{0003}\u{0004}`; old `.xls` files and password-protected `.xlsx`
    files are both OLE2 compound files (`D0 CF 11 E0 A1 B1 1A E1`).
  - Before anything is inflated, the sizes in the ZIP directory are checked,
    as Apache POI does: the total uncompressed size has a maximum, and large
    entries that compress better than `minRatio` (e.g. 100:1) are refused as
    likely zip bombs.
  - Part names are looked up ignoring case, as OPC requires.

@docs Archive, ArchiveError, Limits, open
@docs PartError, readText, has, paths

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Dict exposing (Dict)
import Zip
import Zip.Entry as Entry exposing (Entry)


{-| -}
type Archive
    = Archive (Dict String Entry)


{-| -}
type ArchiveError
    = NotAZip
    | OleCompoundFile
    | TooLarge String


{-| -}
type alias Limits =
    { maxUncompressedBytes : Int
    , minRatio : Float
    }


{-| Entries smaller than this are never treated as zip bombs: small, very
repetitive XML legitimately compresses very well.
-}
ratioCheckFrom : Int
ratioCheckFrom =
    1024 * 1024


{-| -}
open : Limits -> Bytes -> Result ArchiveError Archive
open limits bytes =
    case signature bytes of
        Zip ->
            case Zip.fromBytes bytes of
                Nothing ->
                    Err NotAZip

                Just zip ->
                    let
                        entries =
                            List.filter (not << Entry.isDirectory) (Zip.entries zip)

                        total =
                            List.foldl (\e acc -> acc + Entry.extractedSize e) 0 entries
                    in
                    if total > limits.maxUncompressedBytes then
                        Err (TooLarge ("The file would take " ++ megabytes total ++ " uncompressed, more than the limit of " ++ megabytes limits.maxUncompressedBytes ++ "."))

                    else
                        let
                            suspicious =
                                List.filter
                                    (\e ->
                                        Entry.extractedSize e
                                            >= ratioCheckFrom
                                            && toFloat (Entry.compressedSize e)
                                            / toFloat (Entry.extractedSize e)
                                            < limits.minRatio
                                    )
                                    entries
                        in
                        case suspicious of
                            e :: _ ->
                                Err (TooLarge ("The part " ++ Entry.path e ++ " compresses suspiciously well (" ++ megabytes (Entry.extractedSize e) ++ " from " ++ String.fromInt (Entry.compressedSize e) ++ " bytes); it may be a zip bomb."))

                            [] ->
                                Ok (Archive (Dict.fromList (List.map (\e -> ( String.toLower (Entry.path e), e )) entries)))

        Ole ->
            Err OleCompoundFile

        Unknown ->
            Err NotAZip


megabytes : Int -> String
megabytes n =
    String.fromInt (n // (1024 * 1024)) ++ " MB"


type Signature
    = Zip
    | Ole
    | Unknown


signature : Bytes -> Signature
signature bytes =
    case Decode.decode (Decode.map2 Tuple.pair (Decode.unsignedInt32 Bytes.BE) (Decode.unsignedInt32 Bytes.BE)) bytes of
        Just ( first, second ) ->
            if first == 0x504B0304 then
                Zip

            else if first == 0xD0CF11E0 && second == 0xA1B11AE1 then
                Ole

            else
                Unknown

        Nothing ->
            case Decode.decode (Decode.unsignedInt32 Bytes.BE) bytes of
                Just 0x504B0304 ->
                    Zip

                _ ->
                    Unknown


{-| Whether a part exists, ignoring case.
-}
has : String -> Archive -> Bool
has path (Archive entries) =
    Dict.member (String.toLower (stripSlash path)) entries


{-| The part names, as stored.
-}
paths : Archive -> List String
paths (Archive entries) =
    List.map Entry.path (Dict.values entries)


stripSlash : String -> String
stripSlash path =
    if String.startsWith "/" path then
        String.dropLeft 1 path

    else
        path


{-| Why a part could not be read as text. `PartCorrupt` explains itself.
-}
type PartError
    = PartMissing
    | PartCorrupt String
    | PartUtf16


{-| A part decoded as UTF-8 text, without a byte-order mark.
-}
readText : String -> Archive -> Result PartError String
readText path (Archive entries) =
    case Dict.get (String.toLower (stripSlash path)) entries of
        Nothing ->
            Err PartMissing

        Just entry ->
            case Entry.toBytes entry of
                Err Entry.IntegrityError ->
                    Err (PartCorrupt "its checksum does not match")

                Err (Entry.UnsupportedCompression method _) ->
                    Err (PartCorrupt ("it uses an unsupported compression method (" ++ String.fromInt method ++ ")"))

                Err _ ->
                    Err (PartCorrupt "it could not be decompressed")

                Ok bytes ->
                    decodeUtf8 bytes


decodeUtf8 : Bytes -> Result PartError String
decodeUtf8 bytes =
    case Decode.decode (Decode.unsignedInt16 Bytes.BE) bytes of
        Just 0xFFFE ->
            Err PartUtf16

        Just 0xFEFF ->
            Err PartUtf16

        _ ->
            case Decode.decode (Decode.string (Bytes.width bytes)) bytes of
                Just text ->
                    Ok
                        (if String.startsWith "\u{FEFF}" text then
                            String.dropLeft 1 text

                         else
                            text
                        )

                Nothing ->
                    Err (PartCorrupt "it is not valid UTF-8")
