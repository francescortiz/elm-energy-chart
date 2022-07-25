module EChart.Utils exposing (..)

import DateFormat
import EChart.Types exposing (ChartConfig, TimeInterval(..))
import Round
import Time exposing (millisToPosix)



-- CONSTANTS


millis1Second : Int
millis1Second =
    1000


millis1Minute : Int
millis1Minute =
    60 * millis1Second


millis1Hour : Int
millis1Hour =
    60 * millis1Minute


millis1Day : Int
millis1Day =
    24 * millis1Hour


millis1Month : Int
millis1Month =
    31 * millis1Day


millis1Year : Int
millis1Year =
    365 * millis1Day



-- TIME


getDurationFromTimeInterval : TimeInterval -> Int
getDurationFromTimeInterval timeInterval =
    case timeInterval of
        Year ->
            millis1Year

        Month ->
            millis1Month

        Day ->
            millis1Day

        Hour ->
            millis1Hour


dateTimeTickFormatter : Time.Zone -> ChartConfig -> (Float -> String)
dateTimeTickFormatter timeZone chartConfig =
    let
        preComputation =
            xAxisTickPreComputerFormatter chartConfig.xTickInterval
    in
    \value ->
        DateFormat.format preComputation timeZone (millisToPosix (round value))


xAxisTickPreComputerFormatter : TimeInterval -> List DateFormat.Token
xAxisTickPreComputerFormatter xTickInterval =
    case xTickInterval of
        Year ->
            [ DateFormat.yearNumber ]

        Month ->
            [ DateFormat.monthNameAbbreviated ]

        Day ->
            [ DateFormat.dayOfMonthNumber
            , DateFormat.text " "
            , DateFormat.monthNameAbbreviated
            ]

        Hour ->
            [ DateFormat.dayOfMonthNumber
            , DateFormat.text " "
            , DateFormat.monthNameAbbreviated
            , DateFormat.text " "
            , DateFormat.hourMilitaryNumber
            , DateFormat.text "h"
            ]



-- UNITS


physicalTickFormatterHelper : (Float -> ( Float, Int, String )) -> String -> ChartConfig -> (Float -> String)
physicalTickFormatterHelper formatter units chartConfig =
    let
        preComputation =
            chartConfig.yTicks
                |> List.map .tickValue
                |> List.filter (\x -> x > 0)
                |> List.map (\x -> abs x)
                |> List.sort
                |> List.minimum
                |> Maybe.map
                    (\minimumValue ->
                        formatter minimumValue
                    )
                |> Maybe.withDefault ( 0, 0, "" )

        ( divider, decimals, magnitudeStr ) =
            preComputation
    in
    \value ->
        Round.round decimals (value / divider) ++ " " ++ magnitudeStr ++ units


physicalTickFormatter : String -> ChartConfig -> (Float -> String)
physicalTickFormatter =
    physicalTickFormatterHelper internationalSystemTickPreComputerFormatter


physicalTickFormatterForVolume : String -> ChartConfig -> (Float -> String)
physicalTickFormatterForVolume =
    physicalTickFormatterHelper internationalSystemTickPreComputerFormatterForVolume


internationalSystemTickPreComputerFormatterHelper : { e3 : String, e6 : String, e9 : String, e12 : String } -> Float -> ( Float, Int, String )
internationalSystemTickPreComputerFormatterHelper { e3, e6, e9, e12 } minimumValue =
    if minimumValue >= 0 && minimumValue < 200 then
        ( 1, 0, "" )

    else if minimumValue >= 200 && minimumValue < 2000 then
        ( 1000, 2, e3 )

    else if minimumValue >= 2000 && minimumValue < 20000 then
        ( 1000, 1, e3 )

    else if minimumValue >= 20000 && minimumValue < 200000 then
        ( 1000, 0, e3 )

    else if minimumValue >= 200000 && minimumValue < 2000000 then
        ( 1000000, 2, e6 )

    else if minimumValue >= 2000000 && minimumValue < 20000000 then
        ( 1000000, 1, e6 )

    else if minimumValue >= 20000000 && minimumValue < 200000000 then
        ( 1000000, 0, e6 )

    else if minimumValue >= 200000000 && minimumValue < 2000000000 then
        ( 1000000000, 2, e9 )

    else if minimumValue >= 2000000000 && minimumValue < 20000000000 then
        ( 1000000000, 1, e9 )

    else if minimumValue >= 20000000000 && minimumValue < 200000000000 then
        ( 1000000000, 0, e9 )

    else
        ( 1000000000000, 2, e12 )


internationalSystemTickPreComputerFormatter : Float -> ( Float, Int, String )
internationalSystemTickPreComputerFormatter =
    internationalSystemTickPreComputerFormatterHelper
        { e3 = "k"
        , e6 = "M"
        , e9 = "G"
        , e12 = "T"
        }


internationalSystemTickPreComputerFormatterForVolume : Float -> ( Float, Int, String )
internationalSystemTickPreComputerFormatterForVolume =
    internationalSystemTickPreComputerFormatterHelper
        { e3 = "da"
        , e6 = "h"
        , e9 = "k"
        , e12 = "dak"
        }
