module Chart.Elements.XAxis exposing (..)

import Chart.Types exposing (ChartConfig, ElementDefinition, Padding)
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


render : Options -> ChartConfig -> Svg msg
render options chartConfig =
    text_ [] [ text "X AXIS" ]



-- MAIN


createElement : Options -> ElementDefinition msg
createElement options =
    { contributeToPadding = contributeToPadding options
    , contributeToMaxYTicks = always Nothing
    , render = render options
    }
