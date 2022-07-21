module EChart.Elements exposing (..)

import EChart.Elements.Hover as Hover
import EChart.Elements.XAxis as XAxis
import EChart.Elements.YAxis as YAxis
import EChart.Types exposing (ElementDefinition)


xAxis : XAxis.Options -> ElementDefinition msg
xAxis =
    XAxis.createElement


yAxis : YAxis.Options -> ElementDefinition msg
yAxis =
    YAxis.createElement


hover : Hover.Options msg -> ElementDefinition msg
hover =
    Hover.createElement
