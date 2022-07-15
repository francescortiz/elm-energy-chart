module EChart.Types exposing (..)

import Scale exposing (ContinuousScale)
import Svg exposing (Svg)


type alias Padding =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


type alias InternalDatum =
    --
    ( ( Float -- x0
      , Float -- x1
      )
    , Maybe
        ( Float -- y0
        , Float -- y1
        , Float -- extend: warning. It assumes that either both y0 and y1 are positive or zero, or both are negative or zero.
        )
    )


type alias ChartTick =
    { tickValue : Float, tickPosition : Float }


type alias ChartConfig =
    { -- Canvas properties
      width : Float
    , height : Float
    , padding : Padding
    , contentMinX : Float
    , contentMinY : Float
    , contentWidth : Float
    , contentHeight : Float

    -- Domain properties
    , xScale : ContinuousScale Float
    , yScale : ContinuousScale Float
    , xScaleConvert : Float -> Float
    , yScaleConvert : Float -> Float
    , xScaleInvert : Float -> Float
    , yScaleInvert : Float -> Float
    , minYScaled : Float
    , maxYScaled : Float
    , xTicks : List ChartTick
    , yTicks : List ChartTick
    }


type alias ElementDefinition msg =
    { contributeToPadding : Padding
    , contributeToMaxXTicks : Float -> Maybe Int
    , contributeToMaxYTicks : Float -> Maybe Int
    , render : ChartConfig -> Svg msg
    }
