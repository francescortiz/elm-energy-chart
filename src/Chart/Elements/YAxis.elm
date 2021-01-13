module Chart.Elements.YAxis exposing (..)

import Chart.Types exposing (ChartConfig, Padding)
import TypedSvg exposing (text_)
import TypedSvg.Core exposing (Svg, text)



-- CONSTANTS


size : Float
size =
    20


tickHeight : Float
tickHeight =
    150



-- TYPES


type Placement
    = Inside
    | Outside


type Position
    = Left
    | Right


type alias Options =
    { placement : Placement
    , position : Position
    }



-- LOGIC


contributeToPadding : Options -> Padding
contributeToPadding { placement, position } =
    case placement of
        Inside ->
            Padding 0 0 0 0

        Outside ->
            case position of
                Left ->
                    Padding 0 0 0 size

                Right ->
                    Padding 0 size 0 0


contributeToMaxYTicks : Float -> Int
contributeToMaxYTicks heightInPx =
    heightInPx
        / tickHeight
        |> floor



-- RENDER


yAxis : ChartConfig -> Options -> Svg msg
yAxis chartConfig options =
    text_ [] [ text "Y AXIS" ]
