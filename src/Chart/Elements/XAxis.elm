module Chart.Elements.XAxis exposing (..)

import Chart.Types exposing (ChartConfig, Padding)
import TypedSvg exposing (text_)
import TypedSvg.Core exposing (Svg, text)


type Renderer
    = TimeSeries


type alias Options =
    { renderer : Renderer
    }


contributeToPadding : Options -> Padding
contributeToPadding options =
    Padding 0 0 20 0



-- RENDER


yAxis : ChartConfig -> Options -> Svg msg
yAxis chartConfig options =
    text_ [] [ text "Y AXIS" ]
