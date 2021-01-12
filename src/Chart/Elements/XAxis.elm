module Chart.Elements.XAxis exposing (..)

import Chart.Types exposing (Padding)


type Renderer
    = TimeSeries


type alias Options =
    { renderer : Renderer
    }


contributeToPadding : Options -> Padding
contributeToPadding options =
    Padding 0 0 20 0
