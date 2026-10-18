module Xlsx.Date exposing
    ( DateTime, dateTime, midnight
    , fromPosix, toPosix
    , DateSystem(..)
    , dateToSerial, dateTimeToSerial, serialToDate, serialToDateTime
    , minDate, maxDate
    )

{-| Excel stores dates as **serial numbers**: the number of days since an
epoch, with the time of day as the fraction. This module converts between
those numbers and [`justinmimbs/date`](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
values.

You only need this module to work with raw serial numbers. `Xlsx.Write` and
`Xlsx.Read` already convert date cells for you.


# Date-times

Excel has no time zones: a cell holds the wall-clock time someone typed. So
a date-time here is a local calendar date plus a time of day. Convert to and
from `Time.Posix` with an explicit `Time.Zone`.

@docs DateTime, dateTime, midnight
@docs fromPosix, toPosix


# Serial numbers

@docs DateSystem
@docs dateToSerial, dateTimeToSerial, serialToDate, serialToDateTime
@docs minDate, maxDate

-}

import Date exposing (Date)
import Time
import Time.Extra


{-| A calendar date with a time of day, in local wall-clock time.
-}
type alias DateTime =
    { date : Date
    , hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    }


{-| Build a `DateTime` from a date, hours, minutes and seconds. Values out of
range are clamped (hours to 0–23, minutes and seconds to 0–59).

    import Date
    import Time exposing (Month(..))

    dateTime (Date.fromCalendarDate 2026 Nov 13) 9 30 0
    --> { date = Date.fromCalendarDate 2026 Nov 13, hour = 9, minute = 30, second = 0, millisecond = 0 }

-}
dateTime : Date -> Int -> Int -> Int -> DateTime
dateTime date hour minute second =
    { date = date
    , hour = clamp 0 23 hour
    , minute = clamp 0 59 minute
    , second = clamp 0 59 second
    , millisecond = 0
    }


{-| The start of a day.
-}
midnight : Date -> DateTime
midnight date =
    { date = date, hour = 0, minute = 0, second = 0, millisecond = 0 }


{-| The wall-clock time of a moment in a time zone.
-}
fromPosix : Time.Zone -> Time.Posix -> DateTime
fromPosix zone posix =
    let
        parts =
            Time.Extra.posixToParts zone posix
    in
    { date = Date.fromCalendarDate parts.year parts.month parts.day
    , hour = parts.hour
    , minute = parts.minute
    , second = parts.second
    , millisecond = parts.millisecond
    }


{-| The moment a wall-clock time happens in a time zone.
-}
toPosix : Time.Zone -> DateTime -> Time.Posix
toPosix zone dt =
    Time.Extra.partsToPosix zone
        { year = Date.year dt.date
        , month = Date.month dt.date
        , day = Date.day dt.date
        , hour = dt.hour
        , minute = dt.minute
        , second = dt.second
        , millisecond = dt.millisecond
        }


{-| Which epoch a workbook counts its serial numbers from.

  - `Excel1900`: the default everywhere. Serial 1 is 1900-01-01.
  - `Excel1904`: used by old Mac workbooks. Serial 0 is 1904-01-01.

The 1900 system copies a Lotus 1-2-3 bug: it treats 1900 as a leap year, so
serial 60 is the date 1900-02-29, which does not exist. This module reads
serial 60 as 1900-03-01 and never writes it. Every date from 1900-03-01 on
gets the same serial as in Excel.

-}
type DateSystem
    = Excel1900
    | Excel1904


{-| The first date Excel can show: 1900-01-01.
-}
minDate : Date
minDate =
    Date.fromCalendarDate 1900 Time.Jan 1


{-| The last date Excel can show: 9999-12-31.
-}
maxDate : Date
maxDate =
    Date.fromCalendarDate 9999 Time.Dec 31


{-| The serial number of a date.

    import Date
    import Time exposing (Month(..))

    dateToSerial Excel1900 (Date.fromCalendarDate 2000 Jan 1)
    --> 36526

    dateToSerial Excel1900 (Date.fromCalendarDate 1900 Mar 1)
    --> 61

    dateToSerial Excel1904 (Date.fromCalendarDate 1904 Jan 1)
    --> 0

-}
dateToSerial : DateSystem -> Date -> Int
dateToSerial system date =
    let
        rd =
            Date.toRataDie date
    in
    case system of
        Excel1900 ->
            if rd < leapBugRataDie then
                rd - Date.toRataDie (Date.fromCalendarDate 1899 Time.Dec 31)

            else
                rd - epoch1900

        Excel1904 ->
            rd - epoch1904


{-| The serial number of a date-time: the day plus the fraction of the day.

    import Date
    import Time exposing (Month(..))

    dateTimeToSerial Excel1900 (dateTime (Date.fromCalendarDate 2000 Jan 1) 12 0 0)
    --> 36526.5

-}
dateTimeToSerial : DateSystem -> DateTime -> Float
dateTimeToSerial system dt =
    toFloat (dateToSerial system dt.date) + toFloat (millisOfDay dt) / toFloat millisPerDay


{-| The date of a serial number. The time of day is dropped. Gives `Nothing`
for negative numbers, NaN, infinity, and serials past 9999-12-31.
-}
serialToDate : DateSystem -> Float -> Maybe Date
serialToDate system serial =
    serialToDateTime system serial |> Maybe.map .date


{-| The date-time of a serial number, rounded to the nearest millisecond.
Gives `Nothing` for negative numbers, NaN, infinity, and serials past
9999-12-31.

In the 1900 system, serials below 1 (a time with no date) fall on
1899-12-31, which Excel shows as "1900-01-00".

-}
serialToDateTime : DateSystem -> Float -> Maybe DateTime
serialToDateTime system serial =
    if isNaN serial || isInfinite serial || serial < 0 then
        Nothing

    else
        let
            wholeDays =
                floor serial

            millis =
                round ((serial - toFloat wholeDays) * toFloat millisPerDay)

            ( days, ms ) =
                if millis >= millisPerDay then
                    ( wholeDays + 1, millis - millisPerDay )

                else
                    ( wholeDays, millis )

            rd =
                case system of
                    Excel1900 ->
                        if days < 61 then
                            -- Serials 1–59 count from 1899-12-31, and the fictional
                            -- 1900-02-29 (serial 60) is read as 1900-03-01.
                            Date.toRataDie (Date.fromCalendarDate 1899 Time.Dec 31) + min days 60

                        else
                            epoch1900 + days

                    Excel1904 ->
                        epoch1904 + days
        in
        if rd > Date.toRataDie maxDate then
            Nothing

        else
            Just
                { date = Date.fromRataDie rd
                , hour = ms // 3600000
                , minute = modBy 60 (ms // 60000)
                , second = modBy 60 (ms // 1000)
                , millisecond = modBy 1000 ms
                }



-- HELPERS


millisPerDay : Int
millisPerDay =
    86400000


millisOfDay : DateTime -> Int
millisOfDay dt =
    ((dt.hour * 60 + dt.minute) * 60 + dt.second) * 1000 + dt.millisecond


epoch1900 : Int
epoch1900 =
    Date.toRataDie (Date.fromCalendarDate 1899 Time.Dec 30)


epoch1904 : Int
epoch1904 =
    Date.toRataDie (Date.fromCalendarDate 1904 Time.Jan 1)


{-| 1900-03-01: from this day on, serials count from 1899-12-30.
-}
leapBugRataDie : Int
leapBugRataDie =
    Date.toRataDie (Date.fromCalendarDate 1900 Time.Mar 1)
