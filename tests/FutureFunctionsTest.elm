module FutureFunctionsTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Xlsx.Internal.FutureFunctions exposing (addPrefixes)


suite : Test
suite =
    describe "Xlsx.Internal.FutureFunctions"
        [ test "classic functions unchanged" <|
            \_ -> Expect.equal "SUM(A1:A3)+VLOOKUP(B1,C:D,2,FALSE)" (addPrefixes "SUM(A1:A3)+VLOOKUP(B1,C:D,2,FALSE)")
        , test "XLOOKUP" <|
            \_ -> Expect.equal "_xlfn.XLOOKUP(A1,B:B,C:C)" (addPrefixes "XLOOKUP(A1,B:B,C:C)")
        , test "dotted names" <|
            \_ -> Expect.equal "_xlfn.STDEV.S(A1:A9)" (addPrefixes "STDEV.S(A1:A9)")
        , test "lowercase" <|
            \_ -> Expect.equal "_xlfn.ifs(A1>0,1)" (addPrefixes "ifs(A1>0,1)")
        , test "nested" <|
            \_ -> Expect.equal "IF(_xlfn.MAXIFS(A:A,B:B,1)>0,_xlfn.CONCAT(\"a\",\"b\"),0)" (addPrefixes "IF(MAXIFS(A:A,B:B,1)>0,CONCAT(\"a\",\"b\"),0)")
        , test "already prefixed" <|
            \_ -> Expect.equal "_xlfn.XLOOKUP(A1,B:B,C:C)" (addPrefixes "_xlfn.XLOOKUP(A1,B:B,C:C)")
        , test "string literal untouched" <|
            \_ -> Expect.equal "\"IFS(\"&A1" (addPrefixes "\"IFS(\"&A1")
        , test "escaped quote in literal" <|
            \_ -> Expect.equal "\"say \"\"IFS(\"\"\"&_xlfn.IFS(A1,1)" (addPrefixes "\"say \"\"IFS(\"\"\"&IFS(A1,1)")
        , test "quoted sheet name untouched" <|
            \_ -> Expect.equal "SUM('IFS(x'!A1)" (addPrefixes "SUM('IFS(x'!A1)")
        , test "FILTER gets the worksheet namespace" <|
            \_ -> Expect.equal "_xlfn._xlws.FILTER(A:A,B:B>0)" (addPrefixes "FILTER(A:A,B:B>0)")
        , test "LET with spaces" <|
            \_ -> Expect.equal "_xlfn.LET(x, 1, x + 1)" (addPrefixes "LET(x, 1, x + 1)")
        ]
