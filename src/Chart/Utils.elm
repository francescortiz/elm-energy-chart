module Chart.Utils exposing (..)

import Chart.Types exposing (ChartConfig)
import DateFormat
import Round
import Scale
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


millis4Days : Int
millis4Days =
    4 * millis1Day



-- TIME


dateTimeTickFormatter : Time.Zone -> ChartConfig -> (Float -> String)
dateTimeTickFormatter timeZone chartConfig =
    let
        ( startMillis, endMillis ) =
            Scale.domain chartConfig.xScale
                |> Tuple.mapBoth round round

        preComputation =
            xAxisTickPreComputerFormatter startMillis endMillis
    in
    \value ->
        DateFormat.format preComputation timeZone (millisToPosix (round value))


xAxisTickPreComputerFormatter : Int -> Int -> List DateFormat.Token
xAxisTickPreComputerFormatter startTime endTime =
    let
        timeSpan =
            endTime - startTime
    in
    if timeSpan < millis1Day then
        [ DateFormat.hourMilitaryNumber
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        ]

    else if timeSpan < millis4Days then
        [ DateFormat.dayOfMonthNumber
        , DateFormat.text " "
        , DateFormat.monthNameAbbreviated
        , DateFormat.text " "
        , DateFormat.hourMilitaryNumber
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        ]

    else
        [ DateFormat.dayOfMonthNumber
        , DateFormat.text " "
        , DateFormat.monthNameAbbreviated
        ]



-- UNITS


physicalTickFormatter : String -> ChartConfig -> (Float -> String)
physicalTickFormatter units chartConfig =
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
                        internationalSystemTickPreComputerFormatter minimumValue
                    )
                |> Maybe.withDefault ( 0, 0, "" )

        ( divider, decimals, magnitudeStr ) =
            preComputation
    in
    \value ->
        Round.round decimals (value / divider) ++ " " ++ magnitudeStr ++ units


internationalSystemTickPreComputerFormatter : Float -> ( Float, Int, String )
internationalSystemTickPreComputerFormatter minimumValue =
    if minimumValue >= 0 && minimumValue < 200 then
        ( 1, 0, "" )

    else if minimumValue >= 200 && minimumValue < 2000 then
        ( 1000, 2, "k" )

    else if minimumValue >= 2000 && minimumValue < 20000 then
        ( 1000, 1, "k" )

    else if minimumValue >= 20000 && minimumValue < 200000 then
        ( 1000, 0, "k" )

    else if minimumValue >= 200000 && minimumValue < 2000000 then
        ( 1000000, 2, "M" )

    else if minimumValue >= 2000000 && minimumValue < 20000000 then
        ( 1000000, 1, "M" )

    else if minimumValue >= 20000000 && minimumValue < 200000000 then
        ( 1000000, 0, "M" )

    else if minimumValue >= 200000000 && minimumValue < 2000000000 then
        ( 1000000000, 2, "G" )

    else if minimumValue >= 2000000000 && minimumValue < 20000000000 then
        ( 1000000000, 1, "G" )

    else if minimumValue >= 20000000000 && minimumValue < 200000000000 then
        ( 1000000000, 0, "G" )

    else
        ( 1000000000000, 2, "T" )
