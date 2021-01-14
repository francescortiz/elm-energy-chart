module Chart.Elements.YAxis exposing (..)

import Chart.Types exposing (ChartConfig, ElementDefinition, Padding)
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


requestedPaddingTop : Float
requestedPaddingTop =
    27


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
            Padding requestedPaddingTop 0 0 0

        Outside ->
            case position of
                Left ->
                    Padding requestedPaddingTop 0 0 size

                Right ->
                    Padding requestedPaddingTop size 0 0


contributeToMaxYTicks : Float -> Maybe Int
contributeToMaxYTicks heightInPx =
    heightInPx
        / yTickRequestHeight
        |> floor
        |> Just



-- RENDER


render : Options -> ChartConfig -> Svg msg
render options chartConfig =
    let
        yTickRectPaddingRight =
            5

        yTickOutsideChart =
            options.placement == Outside

        yTicksXPosition =
            if options.position == Right then
                chartConfig.width
                    - chartConfig.padding.left
                    - yTickRectWidth
                    - yTickRectPaddingRight

            else if yTickOutsideChart then
                -yTickRectWidth - yTickRectPaddingRight

            else
                yTickRectPaddingRight

        drawTick { tickValue, tickPosition } ( tickList, isFirst ) =
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
                        , transform
                            [ Translate
                                chartConfig.padding.left
                                (chartConfig.height
                                    - chartConfig.padding.bottom
                                    - tickPosition
                                )
                            ]
                        ]
                        [ line
                            ([ stroke "var(--border)"
                             , x1
                                (if options.position == Right then
                                    0

                                 else if options.placement == Inside then
                                    0

                                 else
                                    yTicksXPosition
                                )
                             , y1 0.5
                             , x2
                                (chartConfig.width
                                    - chartConfig.padding.left
                                    - (if yTickOutsideChart then
                                        0

                                       else
                                        chartConfig.padding.right
                                      )
                                )
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
                                    , textAnchor AnchorMiddle
                                    , fontSize 12
                                    , fontWeight FontWeightBold
                                    , class [ "charts-common--axis-label" ]
                                    , fill "var(--text)"
                                    , x (yTicksXPosition + yTickRectWidth / 2)
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
        , textAnchor AnchorMiddle
        ]
        (chartConfig.yTicks
            |> List.foldl drawTick ( [], True )
            |> Tuple.first
        )



-- MAIN


createElement : Options -> ElementDefinition msg
createElement options =
    { contributeToPadding = contributeToPadding options
    , contributeToMaxXTicks = always Nothing
    , contributeToMaxYTicks = contributeToMaxYTicks
    , render = render options
    }
