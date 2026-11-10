module SecurityTest exposing (suite)

{-| Hostile and broken files (see `scripts/make_security_fixtures.py`). Each
must give a clear error, or be read harmlessly, and never crash or hang.
-}

import Base64
import Bytes
import Bytes.Encode
import Expect
import Fixtures
import Test exposing (Test, describe, test)
import Xlsx.Read as Read
import Xlsx.Ref as Ref


type Outcome
    = OpenFails Read.Error
    | SheetFails Read.Error
    | Reads (Maybe String)


outcome : String -> Outcome
outcome b64 =
    case Base64.toBytes b64 of
        Nothing ->
            -- the empty file
            outcomeOf (Read.read (Maybe.withDefault emptyBytes (Base64.toBytes "")))

        Just bytes ->
            outcomeOf (Read.read bytes)


emptyBytes : Bytes.Bytes
emptyBytes =
    Bytes.Encode.encode (Bytes.Encode.sequence [])


outcomeOf : Result Read.Error Read.Workbook -> Outcome
outcomeOf opened =
    case opened of
        Err e ->
            OpenFails e

        Ok wb ->
            case Read.firstSheet wb of
                Err e ->
                    SheetFails e

                Ok sheet ->
                    Reads (Read.dimensions sheet |> Maybe.map Ref.rangeToString)


isInvalidXml : String -> Outcome -> Bool
isInvalidXml part result =
    case result of
        OpenFails (Read.InvalidXml e) ->
            e.part == part

        SheetFails (Read.InvalidXml e) ->
            e.part == part

        _ ->
            False


isLimit : Outcome -> Bool
isLimit result =
    case result of
        OpenFails (Read.LimitExceeded _) ->
            True

        _ ->
            False


suite : Test
suite =
    describe "hostile and broken files"
        [ describe "refused"
            [ test "billion laughs in a worksheet" <| \_ -> Expect.equal True (isInvalidXml "xl/worksheets/sheet1.xml" (outcome Fixtures.billionLaughsSheet))
            , test "billion laughs in the workbook" <| \_ -> Expect.equal True (isInvalidXml "xl/workbook.xml" (outcome Fixtures.billionLaughsWorkbook))
            , test "external entity in shared strings" <| \_ -> Expect.equal True (isInvalidXml "xl/sharedStrings.xml" (outcome Fixtures.xxe))
            , test "zip bomb" <| \_ -> Expect.equal True (isLimit (outcome Fixtures.zipBomb))
            , test "damaged entry" <| \_ -> Expect.equal True (isInvalidXml "xl/worksheets/sheet1.xml" (outcome Fixtures.badCrc))
            , test "OLE2 (legacy .xls or encrypted)" <| \_ -> Expect.equal (OpenFails Read.EncryptedOrLegacyXls) (outcome Fixtures.encrypted)
            , test "UTF-16 part" <| \_ -> Expect.equal (SheetFails (Read.UnsupportedEncoding "xl/worksheets/sheet1.xml")) (outcome Fixtures.utf16)
            , test "truncated file" <| \_ -> Expect.equal (OpenFails Read.NotAnXlsxFile) (outcome Fixtures.truncated)
            , test "empty file" <| \_ -> Expect.equal (OpenFails Read.NotAnXlsxFile) (outcome Fixtures.empty)
            , test "not a zip" <| \_ -> Expect.equal (OpenFails Read.NotAnXlsxFile) (outcome Fixtures.notAZip)
            , test "binary workbook" <|
                \_ ->
                    case outcome Fixtures.binary of
                        OpenFails (Read.UnsupportedFormat _) ->
                            Expect.pass

                        other ->
                            Expect.fail (Debug.toString other)
            , test "relationship escaping the package" <| \_ -> Expect.equal (SheetFails (Read.MissingPart "etc/passwd")) (outcome Fixtures.badTargets)
            , test "total size limit" <|
                \_ ->
                    Base64.toBytes Fixtures.simple01
                        |> Maybe.map (Read.readWith { maxUncompressedBytes = 1000, minCompressionRatio = 0.01 } >> outcomeOf >> isLimit)
                        |> Expect.equal (Just True)
            ]
        , describe "read harmlessly"
            [ test "deep nesting is skipped, not recursed into" <| \_ -> Expect.equal (Reads (Just "A1")) (outcome Fixtures.deepNesting)
            , test "malformed workbook XML is read leniently" <| \_ -> Expect.equal (Reads (Just "A1")) (outcome Fixtures.malformedWorkbook)
            , test "rows past Excel's grid are ignored" <| \_ -> Expect.equal (Reads (Just "A1:XFD1048576")) (outcome Fixtures.farCell)
            ]
        ]
