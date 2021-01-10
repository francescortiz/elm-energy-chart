module Chart.Layers.YAxis exposing (..)

import Chart.Types exposing (Padding)



-- CONSTANTS


size : Float
size =
    20



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
