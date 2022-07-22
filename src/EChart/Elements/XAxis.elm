module EChart.Elements.XAxis exposing (..)

import EChart.Types exposing (ChartConfig, ChartTick, ElementDefinition, Padding, TimeInterval(..), XAxisInfo, defaultTimeIntervals)
import EChart.Utils exposing (getDurationFromTimeInterval)
import Svg.Attributes as RawSvg exposing (fill, stroke)
import TypedSvg exposing (g, line, text_)
import TypedSvg.Attributes exposing (class, fontWeight, textAnchor, transform)
import TypedSvg.Attributes.InPx exposing (fontSize, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Paint(..), Transform(..))



-- CONSTANTS


allowedMaxXTicks : Int
allowedMaxXTicks =
    10


xTickRequestWidth : Float
xTickRequestWidth =
    40


tickLineHeight : Float
tickLineHeight =
    5


textExtraY : Float
textExtraY =
    12


rotatedTextExtraPaddingRight : Float
rotatedTextExtraPaddingRight =
    4



-- TYPES


type alias Options =
    { tickFormatter : ChartConfig -> Float -> String
    , paddingLeft : Float
    , paddingBottom : Float
    , intervals : Maybe (List ( TimeInterval, Int ))
    }



-- LOGIC


contributeToPadding : Options -> Padding
contributeToPadding options =
    Padding 0 rotatedTextExtraPaddingRight options.paddingBottom options.paddingLeft


contributeToXTicks : Options -> XAxisInfo -> Maybe ( Int, TimeInterval, Int )
contributeToXTicks options { widthInPx, startTimeInMs, endTimeInMs } =
    let
        maxXTicksThatFit : Int
        maxXTicksThatFit =
            widthInPx
                / xTickRequestWidth
                |> floor

        numXTicks : Int
        numXTicks =
            min maxXTicksThatFit allowedMaxXTicks

        domainDuration : Int
        domainDuration =
            endTimeInMs - startTimeInMs

        intervals : List ( TimeInterval, Int )
        intervals =
            case options.intervals of
                Just intervals_ ->
                    intervals_

                Nothing ->
                    defaultTimeIntervals

        ( bestNumXTicks, bestXTicksTimeInterval, bestXTicksTimeIntervalRepeat ) =
            intervals
                |> List.foldl
                    (\( xTicksTimeInterval, xTicksRepeatInterval ) ( bestNumXTicks_, bestXTicksTimeInterval_, bestXTicksTimeIntervalRepeat_ ) ->
                        let
                            currentTimeIntervalDuration : Int
                            currentTimeIntervalDuration =
                                getDurationFromTimeInterval xTicksTimeInterval

                            currentIntervalNumRepeat : Int
                            currentIntervalNumRepeat =
                                xTicksRepeatInterval

                            currentIntervalDuration : Int
                            currentIntervalDuration =
                                currentTimeIntervalDuration * currentIntervalNumRepeat

                            currentNumTicks : Int
                            currentNumTicks =
                                1 + domainDuration // currentIntervalDuration
                        in
                        if abs (currentNumTicks - numXTicks) <= abs (bestNumXTicks_ - numXTicks) then
                            ( currentNumTicks, xTicksTimeInterval, xTicksRepeatInterval )

                        else
                            ( bestNumXTicks_, bestXTicksTimeInterval_, bestXTicksTimeIntervalRepeat_ )
                    )
                    ( 0, Day, 1 )
    in
    ( bestNumXTicks, bestXTicksTimeInterval, bestXTicksTimeIntervalRepeat )
        |> Just



-- RENDER


render : Options -> ChartConfig -> Svg msg
render options chartConfig =
    let
        tickFormatter =
            options.tickFormatter chartConfig

        drawTick { tickValue, tickPosition } ( tickList, isFirst ) =
            ( tickList
                ++ [ g
                        [ class [ "tick" ]
                        , transform
                            [ Translate
                                (tickPosition + chartConfig.padding.left)
                                (chartConfig.height - chartConfig.padding.bottom)
                            ]
                        ]
                        [ line
                            [ stroke "var(--border)"
                            , x1 0
                            , y1 0
                            , x2 0
                            , y2 tickLineHeight
                            ]
                            []
                        , text_
                            [ class [ "bar-chart-tick" ]
                            , textAnchor AnchorEnd
                            , fontSize 12
                            , fontWeight FontWeightBold
                            , class [ "charts-common--axis-label" ]
                            , fill "var(--text)"
                            , x -2
                            , y (tickLineHeight + textExtraY)
                            , RawSvg.transform "rotate(-20)"
                            ]
                            [ text (tickFormatter tickValue) ]
                        ]
                   ]
            , False
            )
    in
    g
        [ class [ "x-axis" ]
        ]
        (chartConfig.xTicks
            |> List.foldl drawTick ( [], True )
            |> Tuple.first
        )



-- MAIN


createElement : Options -> ElementDefinition msg
createElement options =
    { contributeToPadding = contributeToPadding options
    , contributeToXTicks = contributeToXTicks options
    , contributeToMaxYTicks = always Nothing
    , render = render options
    }
