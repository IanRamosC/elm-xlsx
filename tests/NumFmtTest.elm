module NumFmtTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Xlsx.Internal.NumFmt as NumFmt


dateFormats : List String
dateFormats =
    [ "yyyy-mm-dd"
    , "m/d/yy"
    , "d-mmm-yy"
    , "h:mm AM/PM"
    , "[h]:mm:ss"
    , "mm:ss.0"
    , "[$-409]mmmm d, yyyy"
    , "[$-F800]dddd, mmmm dd, yyyy"
    , "dd/mm/yyyy;@"
    , "[Red]yyyy-mm-dd"
    , "yyyy\\-mm\\-dd"
    , "\"Date: \"yyyy-mm-dd"
    , "[mm]:ss"
    , "YYYY-MM-DD"
    ]


numberFormats : List String
numberFormats =
    [ "General"
    , "general"
    , "0"
    , "0.00"
    , "#,##0.00"
    , "0%"
    , "0.00E+00"
    , "# ?/?"
    , "@"
    , "\"$\"#,##0.00"
    , "[$$-409]#,##0.00"
    , "#,##0.00 \"days\""
    , "0.00\\h"
    , "[Red][<0]0;0"
    , "_(* #,##0_);_(* (#,##0);_(* \"-\"_);_(@_)"
    , "0;\"yes\"d"
    ]


suite : Test
suite =
    describe "Xlsx.Internal.NumFmt"
        [ describe "isDateFormat"
            [ test "date and time formats" <|
                \_ -> Expect.equal [] (List.filter (not << NumFmt.isDateFormat) dateFormats)
            , test "number formats" <|
                \_ -> Expect.equal [] (List.filter NumFmt.isDateFormat numberFormats)
            ]
        , describe "isDateFormatId"
            [ test "built-in date ids" <|
                \_ -> Expect.equal True (List.all NumFmt.isDateFormatId [ 14, 15, 16, 17, 18, 19, 20, 21, 22, 45, 46, 47, 27, 36, 50, 58 ])
            , test "built-in number ids" <|
                \_ -> Expect.equal False (List.any NumFmt.isDateFormatId [ 0, 1, 2, 3, 4, 9, 10, 11, 12, 13, 37, 38, 39, 40, 48, 49 ])
            ]
        , describe "built-in table"
            [ test "exact match reuses the id" <| \_ -> Expect.equal (Just 4) (NumFmt.builtinId "#,##0.00")
            , test "no match" <| \_ -> Expect.equal Nothing (NumFmt.builtinId "yyyy-mm-dd")
            , test "code of an id" <| \_ -> Expect.equal (Just "0%") (NumFmt.builtinCode 9)
            ]
        ]
