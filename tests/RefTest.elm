module RefTest exposing (suite)

import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)
import Xlsx.Ref as Ref


suite : Test
suite =
    describe "Xlsx.Ref"
        [ describe "columnName"
            [ test "single letters" <| \_ -> Expect.equal [ "A", "B", "Z" ] (List.map Ref.columnName [ 1, 2, 26 ])
            , test "two letters" <| \_ -> Expect.equal [ "AA", "AZ", "BA", "ZZ" ] (List.map Ref.columnName [ 27, 52, 53, 702 ])
            , test "three letters" <| \_ -> Expect.equal [ "AAA", "XFD" ] (List.map Ref.columnName [ 703, 16384 ])
            , test "non-positive" <| \_ -> Expect.equal "" (Ref.columnName 0)
            ]
        , describe "columnIndex"
            [ test "XFD is the last column" <| \_ -> Expect.equal (Just 16384) (Ref.columnIndex "XFD")
            , test "lowercase" <| \_ -> Expect.equal (Just 28) (Ref.columnIndex "ab")
            , test "past XFD" <| \_ -> Expect.equal Nothing (Ref.columnIndex "XFE")
            , test "four letters" <| \_ -> Expect.equal Nothing (Ref.columnIndex "AAAA")
            , test "empty" <| \_ -> Expect.equal Nothing (Ref.columnIndex "")
            , test "not letters" <| \_ -> Expect.equal Nothing (Ref.columnIndex "A1")
            , fuzz (Fuzz.intRange 1 16384) "round-trips with columnName" <|
                \n -> Expect.equal (Just n) (Ref.columnIndex (Ref.columnName n))
            ]
        , describe "fromString"
            [ test "B3" <| \_ -> Expect.equal (Just { column = 2, row = 3 }) (Ref.fromString "B3")
            , test "anchored" <| \_ -> Expect.equal (Just { column = 2, row = 3 }) (Ref.fromString "$B$3")
            , test "last cell" <| \_ -> Expect.equal (Just { column = 16384, row = 1048576 }) (Ref.fromString "XFD1048576")
            , test "row too large" <| \_ -> Expect.equal Nothing (Ref.fromString "A1048577")
            , test "row zero" <| \_ -> Expect.equal Nothing (Ref.fromString "A0")
            , test "leading zero" <| \_ -> Expect.equal Nothing (Ref.fromString "A01")
            , test "no row" <| \_ -> Expect.equal Nothing (Ref.fromString "AB")
            , test "no column" <| \_ -> Expect.equal Nothing (Ref.fromString "12")
            , test "junk" <| \_ -> Expect.equal Nothing (Ref.fromString "A1B")
            , fuzz (Fuzz.pair (Fuzz.intRange 1 16384) (Fuzz.intRange 1 1048576)) "round-trips with toString" <|
                \( c, r ) -> Expect.equal (Just { column = c, row = r }) (Ref.fromString (Ref.toString { column = c, row = r }))
            ]
        , describe "ranges"
            [ test "normalizes corners" <|
                \_ ->
                    Ref.rangeFromString "C4:A1"
                        |> Maybe.map Ref.rangeToString
                        |> Expect.equal (Just "A1:C4")
            , test "single cell" <| \_ -> Expect.equal (Just "B2") (Maybe.map Ref.rangeToString (Ref.rangeFromString "B2"))
            , test "too many parts" <| \_ -> Expect.equal Nothing (Ref.rangeFromString "A1:B2:C3")
            , test "contains" <|
                \_ ->
                    Ref.range { column = 1, row = 1 } { column = 3, row = 3 }
                        |> Expect.all
                            [ Ref.rangeContains { column = 2, row = 2 } >> Expect.equal True
                            , Ref.rangeContains { column = 4, row = 2 } >> Expect.equal False
                            ]
            , test "size" <|
                \_ -> Expect.equal { columns = 2, rows = 5 } (Ref.rangeSize (Ref.range { column = 2, row = 1 } { column = 3, row = 5 }))
            ]
        , describe "sheet names"
            [ test "plain" <| \_ -> Expect.equal "Data" (Ref.quoteSheetName "Data")
            , test "space" <| \_ -> Expect.equal "'Q1 Sales'" (Ref.quoteSheetName "Q1 Sales")
            , test "apostrophe" <| \_ -> Expect.equal "'Bob''s'" (Ref.quoteSheetName "Bob's")
            , test "leading digit" <| \_ -> Expect.equal "'2024'" (Ref.quoteSheetName "2024")
            , test "looks like a cell" <| \_ -> Expect.equal "'AB12'" (Ref.quoteSheetName "AB12")
            , test "looks like R1C1" <| \_ -> Expect.equal "'R2'" (Ref.quoteSheetName "R2")
            , test "non-ASCII" <| \_ -> Expect.equal "'Café'" (Ref.quoteSheetName "Café")
            , test "onSheet" <|
                \_ -> Expect.equal "'My Sheet'!A1:B2" (Ref.onSheet "My Sheet" (Ref.range { column = 1, row = 1 } { column = 2, row = 2 }))
            ]
        ]
