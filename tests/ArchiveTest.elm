module ArchiveTest exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Expect
import Test exposing (Test, describe, test)
import Time
import Xlsx.Internal.Archive as Archive
import Zip
import Zip.Entry as Entry


limits : Archive.Limits
limits =
    { maxUncompressedBytes = 256 * 1024 * 1024, minRatio = 0.01 }


meta : String -> Entry.Meta
meta path =
    { path = path, lastModified = ( Time.utc, Time.millisToPosix 315532800000 ), comment = Nothing }


zipOf : List ( String, Bytes, Bool ) -> Bytes
zipOf files =
    files
        |> List.map
            (\( path, bytes, compress ) ->
                if compress then
                    Entry.compress (meta path) bytes

                else
                    Entry.store (meta path) bytes
            )
        |> Zip.fromEntries
        |> Zip.toBytes


utf8 : String -> Bytes
utf8 s =
    Encode.encode (Encode.string s)


suite : Test
suite =
    describe "Xlsx.Internal.Archive"
        [ test "reads a part ignoring case and leading slash" <|
            \_ ->
                zipOf [ ( "xl/Workbook.xml", utf8 "<workbook/>", True ) ]
                    |> Archive.open limits
                    |> Result.toMaybe
                    |> Maybe.map (\a -> ( Archive.readText "/XL/workbook.xml" a, Archive.has "xl/workbook.xml" a ))
                    |> Expect.equal (Just ( Ok "<workbook/>", True ))
        , test "missing part" <|
            \_ ->
                zipOf [ ( "a.xml", utf8 "x", False ) ]
                    |> Archive.open limits
                    |> Result.map (Archive.readText "b.xml")
                    |> Expect.equal (Ok (Err Archive.PartMissing))
        , test "strips a UTF-8 byte-order mark" <|
            \_ ->
                zipOf [ ( "a.xml", utf8 "\u{FEFF}<a/>", False ) ]
                    |> Archive.open limits
                    |> Result.map (Archive.readText "a.xml")
                    |> Expect.equal (Ok (Ok "<a/>"))
        , test "refuses UTF-16" <|
            \_ ->
                zipOf [ ( "a.xml", Encode.encode (Encode.sequence [ Encode.unsignedInt8 0xFF, Encode.unsignedInt8 0xFE, Encode.unsignedInt8 0x3C, Encode.unsignedInt8 0 ]), False ) ]
                    |> Archive.open limits
                    |> Result.map (Archive.readText "a.xml")
                    |> Expect.equal (Ok (Err Archive.PartUtf16))
        , test "OLE2 compound files are recognised" <|
            \_ ->
                Encode.encode (Encode.sequence (List.map Encode.unsignedInt8 [ 0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1, 0, 0, 0, 0 ]))
                    |> Archive.open limits
                    |> Expect.equal (Err Archive.OleCompoundFile)
        , test "random bytes are not a zip" <|
            \_ ->
                utf8 "hello, this is not a spreadsheet"
                    |> Archive.open limits
                    |> Expect.equal (Err Archive.NotAZip)
        , test "empty input is not a zip" <|
            \_ ->
                Encode.encode (Encode.sequence [])
                    |> Archive.open limits
                    |> Expect.equal (Err Archive.NotAZip)
        , test "total size limit" <|
            \_ ->
                zipOf [ ( "a.xml", utf8 (String.repeat 2000 "a"), True ) ]
                    |> Archive.open { limits | maxUncompressedBytes = 1000 }
                    |> isTooLarge
                    |> Expect.equal True
        , test "a large, highly compressed part is refused as a zip bomb" <|
            \_ ->
                zipOf [ ( "bomb.xml", utf8 (String.repeat (2 * 1024 * 1024) "0"), True ) ]
                    |> Archive.open { limits | minRatio = 0.01 }
                    |> isTooLarge
                    |> Expect.equal True
        , test "paths lists parts as stored" <|
            \_ ->
                zipOf [ ( "A.xml", utf8 "", False ), ( "b/c.xml", utf8 "", False ) ]
                    |> Archive.open limits
                    |> Result.map (Archive.paths >> List.sort)
                    |> Expect.equal (Ok [ "A.xml", "b/c.xml" ])
        ]


isTooLarge : Result Archive.ArchiveError a -> Bool
isTooLarge result =
    case result of
        Err (Archive.TooLarge _) ->
            True

        _ ->
            False
