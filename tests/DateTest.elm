module DateTest exposing (suite)

import Date
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)
import Time exposing (Month(..))
import Xlsx.Date as XDate exposing (DateSystem(..))


day : Int -> Month -> Int -> Date.Date
day =
    Date.fromCalendarDate


suite : Test
suite =
    describe "Xlsx.Date"
        [ describe "dateToSerial (1900)"
            [ test "1900-01-01 is 1" <| \_ -> Expect.equal 1 (XDate.dateToSerial Excel1900 (day 1900 Jan 1))
            , test "1900-02-28 is 59" <| \_ -> Expect.equal 59 (XDate.dateToSerial Excel1900 (day 1900 Feb 28))
            , test "1900-03-01 is 61" <| \_ -> Expect.equal 61 (XDate.dateToSerial Excel1900 (day 1900 Mar 1))
            , test "2000-01-01 is 36526" <| \_ -> Expect.equal 36526 (XDate.dateToSerial Excel1900 (day 2000 Jan 1))
            , test "2026-11-13 is 46339" <| \_ -> Expect.equal 46339 (XDate.dateToSerial Excel1900 (day 2026 Nov 13))
            , test "9999-12-31 is 2958465" <| \_ -> Expect.equal 2958465 (XDate.dateToSerial Excel1900 XDate.maxDate)
            ]
        , describe "dateToSerial (1904)"
            [ test "1904-01-01 is 0" <| \_ -> Expect.equal 0 (XDate.dateToSerial Excel1904 (day 1904 Jan 1))
            , test "2000-01-01 is 35064" <| \_ -> Expect.equal 35064 (XDate.dateToSerial Excel1904 (day 2000 Jan 1))
            ]
        , describe "serialToDateTime"
            [ test "60 reads as 1900-03-01" <|
                \_ -> Expect.equal (Just (day 1900 Mar 1)) (XDate.serialToDate Excel1900 60)
            , test "61 is 1900-03-01" <|
                \_ -> Expect.equal (Just (day 1900 Mar 1)) (XDate.serialToDate Excel1900 61)
            , test "59 is 1900-02-28" <|
                \_ -> Expect.equal (Just (day 1900 Feb 28)) (XDate.serialToDate Excel1900 59)
            , test "time only" <|
                \_ ->
                    Expect.equal (Just (XDate.dateTime (day 1899 Dec 31) 6 0 0)) (XDate.serialToDateTime Excel1900 0.25)
            , test "noon" <|
                \_ ->
                    Expect.equal (Just (XDate.dateTime (day 2000 Jan 1) 12 0 0)) (XDate.serialToDateTime Excel1900 36526.5)
            , test "rounds to the millisecond" <|
                \_ ->
                    XDate.serialToDateTime Excel1900 (36526 + 1.6 / 86400000)
                        |> Maybe.map .millisecond
                        |> Expect.equal (Just 2)
            , test "rounding up to midnight moves to the next day" <|
                \_ ->
                    XDate.serialToDateTime Excel1900 (36526 + 0.9999999999)
                        |> Expect.equal (Just (XDate.midnight (day 2000 Jan 2)))
            , test "1904 system" <|
                \_ -> Expect.equal (Just (day 1904 Jan 1)) (XDate.serialToDate Excel1904 0)
            , test "negative" <| \_ -> Expect.equal Nothing (XDate.serialToDate Excel1900 -1)
            , test "NaN" <| \_ -> Expect.equal Nothing (XDate.serialToDate Excel1900 (0 / 0))
            , test "past 9999" <| \_ -> Expect.equal Nothing (XDate.serialToDate Excel1900 2958466)
            ]
        , describe "round trips"
            [ fuzz (Fuzz.intRange 1 2958465) "1900 serial → date → serial (except 60)" <|
                \n ->
                    if n == 60 then
                        Expect.pass

                    else
                        XDate.serialToDate Excel1900 (toFloat n)
                            |> Maybe.map (XDate.dateToSerial Excel1900)
                            |> Expect.equal (Just n)
            , fuzz dateTimeFuzzer "date-time → serial → date-time" <|
                \dt ->
                    XDate.serialToDateTime Excel1900 (XDate.dateTimeToSerial Excel1900 dt)
                        |> Expect.equal (Just dt)
            , fuzz dateTimeFuzzer "1904 date-time → serial → date-time" <|
                \dt ->
                    if Date.year dt.date < 1904 then
                        Expect.pass

                    else
                        XDate.serialToDateTime Excel1904 (XDate.dateTimeToSerial Excel1904 dt)
                            |> Expect.equal (Just dt)
            ]
        , describe "posix"
            [ test "fromPosix in UTC" <|
                \_ ->
                    XDate.fromPosix Time.utc (Time.millisToPosix 1794528000000)
                        |> Expect.equal (XDate.midnight (day 2026 Nov 13))
            , test "toPosix with an offset zone" <|
                \_ ->
                    XDate.toPosix (Time.customZone -180 []) (XDate.midnight (day 2026 Nov 13))
                        |> Time.posixToMillis
                        |> Expect.equal (1794528000000 + 3 * 3600000)
            ]
        ]


dateTimeFuzzer : Fuzz.Fuzzer XDate.DateTime
dateTimeFuzzer =
    Fuzz.map5
        (\rd h m s ms ->
            { date = Date.fromRataDie rd, hour = h, minute = m, second = s, millisecond = ms }
        )
        (Fuzz.intRange (Date.toRataDie (day 1900 Mar 1)) (Date.toRataDie XDate.maxDate))
        (Fuzz.intRange 0 23)
        (Fuzz.intRange 0 59)
        (Fuzz.intRange 0 59)
        (Fuzz.intRange 0 999)
