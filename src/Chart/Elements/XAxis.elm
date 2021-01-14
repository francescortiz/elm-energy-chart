module Chart.Elements.XAxis exposing (..)

import Chart.Types exposing (ChartConfig, ElementDefinition, Padding)
import Scale
import Svg.Attributes exposing (fill, stroke)
import TypedSvg exposing (g, line, rect, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontWeight, strokeDasharray, textAnchor, transform)
import TypedSvg.Attributes.InPx exposing (dy, fontSize, height, rx, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Paint(..), Transform(..))



-- CONSTANTS


requestedPaddingLeft : Float
requestedPaddingLeft =
    40


xTickRequestWidth : Float
xTickRequestWidth =
    40



-- TYPES


type Renderer
    = TimeSeries


type alias Options =
    { renderer : Renderer
    , tickFormatter : Float -> String
    }



-- LOGIC


contributeToPadding : Options -> Padding
contributeToPadding options =
    Padding 0 0 20 requestedPaddingLeft


contributeToMaxXTicks : Float -> Maybe Int
contributeToMaxXTicks widthInPx =
    widthInPx
        / xTickRequestWidth
        |> floor
        |> Just



-- RENDER


render : Options -> ChartConfig -> Svg msg
render options chartConfig =
    let
        drawTick { tickValue, tickPosition } ( tickList, isFirst ) =
            ( (if isFirst then
                [ line
                    [ stroke "var(--border)"
                    , x1 -7
                    , y1 0
                    , x2 -7
                    , y2 1000
                    ]
                    []
                ]

               else
                []
              )
                ++ tickList
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
                            , x1 -8
                            , y1 0.5
                            , x2 -1
                            , y2 0.5
                            ]
                            []
                        , text_
                            [ class [ "bar-chart-tick" ]
                            , textAnchor AnchorEnd
                            , fontSize 12
                            , fontWeight FontWeightBold
                            , class [ "charts-common--axis-label" ]
                            , fill "var(--text)"
                            , x tickPosition
                            , y 0.5
                            ]
                            [ text (options.tickFormatter tickValue) ]
                        ]
                   ]
            , False
            )
    in
    g
        [ fill "none"
        , textAnchor AnchorEnd
        ]
        [ g []
            (chartConfig.xTicks
                |> List.foldl drawTick ( [], True )
                |> Tuple.first
            )
        ]



-- MAIN


createElement : Options -> ElementDefinition msg
createElement options =
    { contributeToPadding = contributeToPadding options
    , contributeToMaxXTicks = contributeToMaxXTicks
    , contributeToMaxYTicks = always Nothing
    , render = render options
    }
