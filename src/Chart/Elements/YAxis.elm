module Chart.Elements.YAxis exposing (..)

import Chart.Types exposing (ChartConfig, Padding)
import Svg.Attributes exposing (fill, stroke)
import TypedSvg exposing (g, line, rect, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontWeight, strokeDasharray, textAnchor, transform)
import TypedSvg.Attributes.InPx exposing (dy, fontSize, height, rx, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Paint(..), Transform(..))



-- CONSTANTS


size : Float
size =
    yTickRectWidth + 10


yTickRequestHeight : Float
yTickRequestHeight =
    40


yTickrequestedPaddingTop : Float
yTickrequestedPaddingTop =
    28


yTickRectWidth : Float
yTickRectWidth =
    80



-- TYPES


type Placement
    = Inside
    | Outside


type Position
    = Left
    | Right


type alias Options =
    { placement : Placement
    , position : Position
    , tickFormatter : Float -> String
    }



-- LOGIC


contributeToPadding : Options -> Padding
contributeToPadding { placement, position } =
    case placement of
        Inside ->
            Padding yTickrequestedPaddingTop 0 0 0

        Outside ->
            case position of
                Left ->
                    Padding yTickrequestedPaddingTop 0 0 size

                Right ->
                    Padding yTickrequestedPaddingTop size 0 0


contributeToMaxYTicks : Float -> Int
contributeToMaxYTicks heightInPx =
    heightInPx
        / yTickRequestHeight
        |> floor



-- RENDER


yAxis : ChartConfig -> Options -> Svg msg
yAxis chartConfig options =
    let
        yTickRectWidthString : String
        yTickRectWidthString =
            yTickRectWidth
                |> String.fromFloat

        yTickRectPaddingRight =
            5

        yTicksXPosition =
            if options.position == Right then
                chartConfig.width - yTickRectWidth - yTickRectPaddingRight

            else
                yTickRectPaddingRight

        yTickOutsideChart =
            options.placement == Outside

        drawTick { tickValue, tickY } ( tickList, isFirst ) =
            let
                dashedAttributes =
                    if not isFirst || yTickOutsideChart then
                        [ strokeDasharray "5" ]

                    else
                        []
            in
            ( tickList
                ++ [ g
                        [ class [ "tick" ]
                        , transform [ Translate 0 (chartConfig.height - chartConfig.padding.bottom - tickY) ]
                        ]
                        [ line
                            ([ stroke "var(--border)"
                             , x1 10000
                             , y1 0.5
                             , x2 0
                             , y2 0.5
                             ]
                                ++ dashedAttributes
                            )
                            []
                        , if tickValue /= 0 || yTickOutsideChart then
                            g []
                                [ rect
                                    [ x yTicksXPosition
                                    , y -24
                                    , width yTickRectWidth
                                    , height 20
                                    , rx 12
                                    , fill "var(--background)"
                                    ]
                                    []
                                , text_
                                    [ class [ "bar-chart-tick" ]
                                    , fontFamily [ "proxima-nova" ]
                                    , textAnchor AnchorMiddle
                                    , fontSize 12
                                    , fontWeight FontWeightBold
                                    , class [ "charts-common--axis-label" ]
                                    , fill "var(--text)"
                                    , x (yTicksXPosition + 40)
                                    , y -10
                                    , dy 0
                                    ]
                                    [ text (options.tickFormatter tickValue) ]
                                ]

                          else
                            g [] []
                        ]
                   ]
            , False
            )
    in
    g
        [ class [ "y-axis" ]
        , fontSize 10
        , fontFamily [ "sans-serif" ]
        , textAnchor AnchorMiddle
        ]
        (chartConfig.yTicks
            |> List.foldl drawTick ( [], True )
            |> Tuple.first
        )
