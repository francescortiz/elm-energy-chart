module EChart.Elements.Hover exposing (..)

import EChart.Types exposing (ChartConfig, ElementDefinition, Padding)
import Html.Events.Extra.Pointer as Pointer
import Svg.Attributes exposing (fill)
import TypedSvg exposing (rect)
import TypedSvg.Attributes exposing (class)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)



-- CONSTANTS
-- TYPES


type alias HoverPoint =
    { domain :
        { x : Float
        , y : Float
        , minX : Float
        , minY : Float
        , width : Float
        , height : Float
        }
    , canvas :
        { x : Float
        , y : Float
        , minX : Float
        , minY : Float
        , width : Float
        , height : Float
        }
    }


type alias Options msg =
    { onMouseMove : Maybe HoverPoint -> msg
    , marker :
        ( Float, Float ) -- (width, height)
        -> ( Float, Float ) -- (x, y)
        -> Svg msg
    }



-- LOGIC


contributeToPadding : Options msg -> Padding
contributeToPadding _ =
    Padding 0 0 0 0



-- RENDER


render : Options msg -> ChartConfig -> Svg msg
render options chartConfig =
    rect
        [ class [ "echart--hover" ]
        , x chartConfig.contentMinX
        , y chartConfig.contentMinY
        , width chartConfig.contentWidth
        , height chartConfig.contentHeight
        , fill "#00000000"
        , Pointer.onMove
            (\event ->
                let
                    canvasPos =
                        event.pointer.offsetPos

                    ( canvasX, canvasY ) =
                        canvasPos

                    padding =
                        chartConfig.padding
                in
                Just
                    { canvas =
                        { x = canvasX
                        , y = canvasY
                        , minX = chartConfig.contentMinX
                        , minY = chartConfig.contentMinY
                        , width = chartConfig.contentWidth
                        , height = chartConfig.contentHeight
                        }
                    , domain =
                        { x = chartConfig.xScaleInvert (canvasX - padding.left)
                        , y = chartConfig.yScaleInvert (canvasY - padding.top)
                        , minX = chartConfig.xScaleInvert (chartConfig.contentMinX - padding.left)
                        , minY = chartConfig.yScaleInvert (chartConfig.contentMinY - padding.top)
                        , width = chartConfig.xScaleInvert (chartConfig.contentWidth - padding.left)
                        , height = chartConfig.yScaleInvert (chartConfig.contentHeight - padding.top)
                        }
                    }
                    |> options.onMouseMove
            )
        , Pointer.onOut (\_ -> options.onMouseMove Nothing)
        ]
        []



-- MAIN


createElement : Options msg -> ElementDefinition msg
createElement options =
    { contributeToPadding = contributeToPadding options
    , contributeToXTicks = always Nothing
    , contributeToMaxYTicks = always Nothing
    , render = render options
    }
