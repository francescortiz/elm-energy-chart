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


type alias XAxisInfo =
    { widthInPx : Float
    , startTimeInMs : Int
    , endTimeInMs : Int
    }


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
    , xTickInterval : TimeInterval
    }


type alias ElementDefinition msg =
    { contributeToPadding : Padding
    , contributeToXTicks : XAxisInfo -> Maybe ( Int, TimeInterval, Int )
    , contributeToMaxYTicks : Float -> Maybe Int
    , render : ChartConfig -> Svg msg
    }


type TimeInterval
    = Year
    | Month
    | Day
    | Hour


defaultTimeIntervals : List ( TimeInterval, Int )
defaultTimeIntervals =
    [ ( Hour, 1 )
    , ( Hour, 3 )
    , ( Hour, 6 )
    , ( Hour, 8 )
    , ( Hour, 12 )
    , ( Day, 1 )
    , ( Day, 2 )
    , ( Day, 3 )
    , ( Day, 4 )
    , ( Day, 5 )
    , ( Day, 6 )
    , ( Day, 7 )
    , ( Day, 14 )
    , ( Month, 1 )
    , ( Month, 2 )
    , ( Month, 3 )
    , ( Month, 4 )
    , ( Month, 6 )
    , ( Year, 1 )
    ]
