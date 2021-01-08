module Main exposing (..)

import Browser
import Chart
import Color
import Html exposing (Html, div, text)
import Time exposing (millisToPosix)
import TypedSvg.Types exposing (Paint(..))


type alias Flags =
    {}


type alias Model =
    {}


type Msg
    = No


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


makeChart : Bool -> Html Msg
makeChart stack =
    Chart.empty
        |> Chart.addDataSet
            { readings =
                [ { start = 0
                  , end = 1000000
                  , a = 1234
                  , b = 22
                  }
                , { start = 1000000
                  , end = 1100000
                  , a = 44
                  , b = 44
                  }
                , { start = 1100000
                  , end = 2000000
                  , a = 2345
                  , b = 12
                  }
                ]
            , seriesList =
                [ { label = "Series A"
                  , accessor = .a >> Just
                  , fill = Paint Color.green
                  , line = Paint Color.black
                  , gapFill = Paint Color.gray
                  }
                , { label = "Series B"
                  , accessor = .b >> Just
                  , fill = Paint Color.blue
                  , line = Paint Color.black
                  , gapFill = Paint Color.gray
                  }
                ]
            , readingType =
                Chart.Accumulated
                    { startAccessor = .start
                    , endAccessor = .end
                    }
            , stack = stack
            }
        |> Chart.addDataSet
            { readings =
                [ { start = 0
                  , end = 1000000
                  , a = 123
                  , b = -2
                  }
                , { start = 1000000
                  , end = 1200000
                  , a = 4
                  , b = -22
                  }
                , { start = 1200000
                  , end = 2000000
                  , a = 234
                  , b = -1
                  }
                ]
            , seriesList =
                [ { label = "Series A"
                  , accessor = .a >> Just
                  , fill = Paint Color.red
                  , line = Paint Color.black
                  , gapFill = Paint Color.gray
                  }
                , { label = "Series B"
                  , accessor = .b >> Just
                  , fill = Paint Color.orange
                  , line = Paint Color.black
                  , gapFill = Paint Color.gray
                  }
                ]
            , readingType =
                Chart.Accumulated
                    { startAccessor = .start
                    , endAccessor = .end
                    }
            , stack = stack
            }
        |> Chart.addDataSet
            { readings =
                [ { start = 500
                  , end = 500
                  , a = 123
                  , b = 100
                  }
                , { start = 800000
                  , end = 800000
                  , a = 4
                  , b = 100
                  }
                , { start = 1200000
                  , end = 1200000
                  , a = 234
                  , b = 1
                  }
                , { start = 1500000
                  , end = 1500000
                  , a = 534
                  , b = 100
                  }
                , { start = 2000000
                  , end = 2000000
                  , a = 734
                  , b = 900
                  }
                ]
            , seriesList =
                [ { label = "Series A"
                  , accessor = .a >> Just
                  , fill = Paint <| Color.rgba 1 0 1 0.5
                  , line = Paint Color.black
                  , gapFill = Paint Color.gray
                  }
                , { label = "Series B"
                  , accessor = .b >> Just
                  , fill = Paint <| Color.rgba 1 1 0 0.5
                  , line = Paint Color.black
                  , gapFill = Paint Color.gray
                  }
                ]
            , readingType =
                Chart.Point
                    { xAccessor = .end
                    }
            , stack = stack
            }
        |> Chart.render
            { size = ( 1000, 400 )
            , startTime = millisToPosix 0
            , endTime = millisToPosix 2000000
            }


view : Model -> Html Msg
view model =
    div []
        [ makeChart False
        , makeChart True
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
