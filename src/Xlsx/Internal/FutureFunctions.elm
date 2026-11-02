module Xlsx.Internal.FutureFunctions exposing (addPrefixes, removePrefixes)

{-| Functions added to Excel after 2007 are stored in the file with a
`_xlfn.` prefix (`_xlfn.XLOOKUP(...)`). Excel adds the prefix itself when you
type a formula, but files written by other programs must add it, or Excel shows
`#NAME?`.

@docs addPrefixes, removePrefixes

-}

import Set exposing (Set)


{-| Add `_xlfn.` (and `_xlfn._xlws.` where Excel needs it) to newer function
names. Text inside string literals and quoted sheet names is left alone, and
names that already have a prefix are not prefixed twice.

    addPrefixes "XLOOKUP(A1,B:B,C:C)" == "_xlfn.XLOOKUP(A1,B:B,C:C)"

    addPrefixes "\"IFS(\"&A1" == "\"IFS(\"&A1"

-}
addPrefixes : String -> String
addPrefixes formula =
    if String.contains "(" formula then
        scan (String.toList formula) Nothing [] []

    else
        formula


{-| Walk the formula. `word` collects the current identifier (reversed),
`prev` is the character before it, used to skip names that follow a `.`
(already prefixed) or belong to a sheet reference.
-}
scan : List Char -> Maybe Char -> List Char -> List String -> String
scan chars prev word acc =
    case chars of
        [] ->
            String.concat (List.reverse (flush word acc))

        '"' :: rest ->
            let
                ( literal, after ) =
                    quoted '"' rest [ '"' ]
            in
            scan after (Just '"') [] (String.fromList literal :: flush word acc)

        '\'' :: rest ->
            let
                ( literal, after ) =
                    quoted '\'' rest [ '\'' ]
            in
            scan after (Just '\'') [] (String.fromList literal :: flush word acc)

        '(' :: rest ->
            let
                name =
                    String.fromList (List.reverse word)

                prefixed =
                    if prev /= Just '.' && prev /= Just '!' then
                        prefixFor name ++ name

                    else
                        name
            in
            scan rest (Just '(') [] ("(" :: prefixed :: acc)

        c :: rest ->
            if Char.isAlphaNum c || c == '.' || c == '_' then
                if List.isEmpty word then
                    -- Remember what came before the identifier.
                    scan rest (lastOf acc) [ c ] acc

                else
                    scan rest prev (c :: word) acc

            else
                scan rest (Just c) [] (String.fromChar c :: flush word acc)


lastOf : List String -> Maybe Char
lastOf acc =
    case acc of
        s :: _ ->
            String.uncons (String.reverse s) |> Maybe.map Tuple.first

        [] ->
            Nothing


flush : List Char -> List String -> List String
flush word acc =
    if List.isEmpty word then
        acc

    else
        String.fromList (List.reverse word) :: acc


{-| Read a quoted literal (the opening quote is already consumed). A doubled
quote stands for one quote inside the literal.
-}
quoted : Char -> List Char -> List Char -> ( List Char, List Char )
quoted quote chars acc =
    case chars of
        [] ->
            ( List.reverse acc, [] )

        c :: d :: rest ->
            if c == quote && d == quote then
                quoted quote rest (d :: c :: acc)

            else if c == quote then
                ( List.reverse (c :: acc), d :: rest )

            else
                quoted quote (d :: rest) (c :: acc)

        [ c ] ->
            ( List.reverse (c :: acc), [] )


prefixFor : String -> String
prefixFor name =
    let
        upper =
            String.toUpper name
    in
    if Set.member upper worksheetFunctions then
        "_xlfn._xlws."

    else if Set.member upper futureFunctions then
        "_xlfn."

    else
        ""


{-| These two need an extra `_xlws.` namespace.
-}
worksheetFunctions : Set String
worksheetFunctions =
    Set.fromList [ "FILTER", "SORT" ]


{-| From XlsxWriter's list of "future functions", plus functions added to
Excel 365 since.
-}
futureFunctions : Set String
futureFunctions =
    Set.fromList
        [ "ACOT"
        , "ACOTH"
        , "AGGREGATE"
        , "ANCHORARRAY"
        , "ARABIC"
        , "ARRAYTOTEXT"
        , "BASE"
        , "BETA.DIST"
        , "BETA.INV"
        , "BINOM.DIST"
        , "BINOM.DIST.RANGE"
        , "BINOM.INV"
        , "BITAND"
        , "BITLSHIFT"
        , "BITOR"
        , "BITRSHIFT"
        , "BITXOR"
        , "BYCOL"
        , "BYROW"
        , "CEILING.MATH"
        , "CEILING.PRECISE"
        , "CHISQ.DIST"
        , "CHISQ.DIST.RT"
        , "CHISQ.INV"
        , "CHISQ.INV.RT"
        , "CHISQ.TEST"
        , "CHOOSECOLS"
        , "CHOOSEROWS"
        , "COMBINA"
        , "CONCAT"
        , "CONFIDENCE.NORM"
        , "CONFIDENCE.T"
        , "COT"
        , "COTH"
        , "COVARIANCE.P"
        , "COVARIANCE.S"
        , "CSC"
        , "CSCH"
        , "DAYS"
        , "DECIMAL"
        , "DROP"
        , "ECMA.CEILING"
        , "ERF.PRECISE"
        , "ERFC.PRECISE"
        , "EXPAND"
        , "EXPON.DIST"
        , "F.DIST"
        , "F.DIST.RT"
        , "F.INV"
        , "F.INV.RT"
        , "F.TEST"
        , "FILTERXML"
        , "FLOOR.MATH"
        , "FLOOR.PRECISE"
        , "FORECAST.ETS"
        , "FORECAST.ETS.CONFINT"
        , "FORECAST.ETS.SEASONALITY"
        , "FORECAST.ETS.STAT"
        , "FORECAST.LINEAR"
        , "FORMULATEXT"
        , "GAMMA"
        , "GAMMA.DIST"
        , "GAMMA.INV"
        , "GAMMALN.PRECISE"
        , "GAUSS"
        , "HSTACK"
        , "HYPGEOM.DIST"
        , "IFNA"
        , "IFS"
        , "IMAGE"
        , "IMCOSH"
        , "IMCOT"
        , "IMCSC"
        , "IMCSCH"
        , "IMSEC"
        , "IMSECH"
        , "IMSINH"
        , "IMTAN"
        , "ISFORMULA"
        , "ISO.CEILING"
        , "ISOMITTED"
        , "ISOWEEKNUM"
        , "LAMBDA"
        , "LET"
        , "LOGNORM.DIST"
        , "LOGNORM.INV"
        , "MAKEARRAY"
        , "MAP"
        , "MAXIFS"
        , "MINIFS"
        , "MODE.MULT"
        , "MODE.SNGL"
        , "MUNIT"
        , "NEGBINOM.DIST"
        , "NETWORKDAYS.INTL"
        , "NORM.DIST"
        , "NORM.INV"
        , "NORM.S.DIST"
        , "NORM.S.INV"
        , "NUMBERVALUE"
        , "PDURATION"
        , "PERCENTILE.EXC"
        , "PERCENTILE.INC"
        , "PERCENTRANK.EXC"
        , "PERCENTRANK.INC"
        , "PERMUTATIONA"
        , "PHI"
        , "POISSON.DIST"
        , "QUARTILE.EXC"
        , "QUARTILE.INC"
        , "QUERYSTRING"
        , "RANDARRAY"
        , "RANK.AVG"
        , "RANK.EQ"
        , "REDUCE"
        , "RRI"
        , "SCAN"
        , "SEC"
        , "SECH"
        , "SEQUENCE"
        , "SHEET"
        , "SHEETS"
        , "SKEW.P"
        , "SORTBY"
        , "STDEV.P"
        , "STDEV.S"
        , "SWITCH"
        , "T.DIST"
        , "T.DIST.2T"
        , "T.DIST.RT"
        , "T.INV"
        , "T.INV.2T"
        , "T.TEST"
        , "TAKE"
        , "TEXTAFTER"
        , "TEXTBEFORE"
        , "TEXTJOIN"
        , "TEXTSPLIT"
        , "TOCOL"
        , "TOROW"
        , "UNICHAR"
        , "UNICODE"
        , "UNIQUE"
        , "VALUETOTEXT"
        , "VAR.P"
        , "VAR.S"
        , "VSTACK"
        , "WEBSERVICE"
        , "WEIBULL.DIST"
        , "WORKDAY.INTL"
        , "WRAPCOLS"
        , "WRAPROWS"
        , "XLOOKUP"
        , "XMATCH"
        , "XOR"
        , "Z.TEST"
        ]


{-| Remove the storage prefixes (`_xlfn.`, `_xlws.`, `_xlpm.` for LAMBDA
parameters) so a formula reads as it is typed. Text in string literals is
left alone.

    removePrefixes "_xlfn._xlws.SORT(A1:A9)" == "SORT(A1:A9)"

-}
removePrefixes : String -> String
removePrefixes formula =
    if String.contains "_xl" formula then
        formula
            |> String.split "\""
            |> List.indexedMap
                (\i part ->
                    if modBy 2 i == 0 then
                        part
                            |> String.replace "_xlfn." ""
                            |> String.replace "_xlws." ""
                            |> String.replace "_xlpm." ""

                    else
                        part
                )
            |> String.join "\""

    else
        formula
