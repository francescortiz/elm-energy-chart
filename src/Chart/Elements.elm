module Chart.Elements exposing (..)

import Chart.Elements.XAxis as XAxis
import Chart.Elements.YAxis as YAxis
import Chart.Types exposing (ElementDefinition)


xAxis : XAxis.Options -> ElementDefinition msg
xAxis =
    XAxis.createElement


yAxis : YAxis.Options -> ElementDefinition msg
yAxis =
    YAxis.createElement
