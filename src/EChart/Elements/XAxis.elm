module EChart.Elements.XAxis exposing (..)

import EChart.Types exposing (ChartConfig, ElementDefinition, Padding)
import Svg.Attributes as RawSvg exposing (fill, stroke)
import TypedSvg exposing (g, line, text_)
import TypedSvg.Attributes exposing (class, fontWeight, textAnchor, transform)
import TypedSvg.Attributes.InPx exposing (fontSize, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Paint(..), Transform(..))



-- CONSTANTS


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
    }



-- LOGIC


contributeToPadding : Options -> Padding
contributeToPadding options =
    Padding 0 rotatedTextExtraPaddingRight options.paddingBottom options.paddingLeft


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
    , contributeToMaxXTicks = contributeToMaxXTicks
    , contributeToMaxYTicks = always Nothing
    , render = render options
    }
