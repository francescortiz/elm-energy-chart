module Main exposing (..)

import Browser
import Chart exposing (AccumulatedLayers, Element(..), PointLayer(..))
import Chart.Elements.XAxis as X
import Chart.Elements.YAxis as YAxis
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
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
                  , b = Just 22
                  }
                , { start = 1000000
                  , end = 1100000
                  , a = 44
                  , b = Nothing
                  }
                , { start = 1100000
                  , end = 2000000
                  , a = 2345
                  , b = Just 12
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
                  , accessor = .b
                  , fill = Paint Color.blue
                  , line = Paint Color.black
                  , gapFill = Paint Color.gray
                  }
                ]
            , readingType =
                Chart.Accumulated
                    { startAccessor = .start
                    , endAccessor = .end
                    , layers =
                        { solid = True
                        , gaps = False
                        }
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
                [ { label = "Series B"
                  , accessor = .b >> Just
                  , fill = Paint Color.orange
                  , line = Paint Color.black
                  , gapFill = Paint Color.gray
                  }
                , { label = "Series A"
                  , accessor = .a >> Just
                  , fill = Paint Color.red
                  , line = Paint Color.black
                  , gapFill = Paint Color.gray
                  }
                ]
            , readingType =
                Chart.Accumulated
                    { startAccessor = .start
                    , endAccessor = .end
                    , layers =
                        { solid = True
                        , gaps = True
                        }
                    }
            , stack = stack
            }
        |> Chart.addDataSet
            { readings =
                [ { time = 500
                  , a = 123
                  , b = Just 100
                  }
                , { time = 400000
                  , a = 4
                  , b = Just 2890
                  }
                , { time = 800000
                  , a = 420
                  , b = Just 2000
                  }
                , { time = 1200000
                  , a = 234
                  , b = Nothing
                  }
                , { time = 1500000
                  , a = 534
                  , b = Just 100
                  }
                , { time = 1750000
                  , a = 534
                  , b = Just 80
                  }
                , { time = 2000000
                  , a = 734
                  , b = Just 900
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
                  , accessor = .b
                  , fill = Paint <| Color.rgba 1 1 0 0.5
                  , line = Paint Color.black
                  , gapFill = Paint Color.gray
                  }
                ]
            , readingType =
                Chart.Point
                    { xAccessor = .time
                    , layers =
                        [ LineLayer
                        , FillLayer
                        ]
                    }
            , stack = stack
            }
        |> Chart.add
            (YAxis
                { placement = YAxis.Outside
                , position = YAxis.Left
                , tickFormatter = String.fromFloat
                }
            )
        |> Chart.add
            (XAxis
                { renderer = X.TimeSeries
                }
            )
        |> Chart.render
            { size = ( 1000, 400 )
            , startTime = millisToPosix 0
            , endTime = millisToPosix 2000000
            }


view : Model -> Html Msg
view model =
    div []
        [ Html.node "style" [] [ text """
        :root {
            --border: red;
            --background: orange;
            --text: white;
        }
        div {
            background: #eee;
            padding: 10px;
        }
        svg {
            background: #fff;
        }
        """ ]
        , div [ style "width" "1000px", style "margin" "auto", style "margin" "auto" ]
            [ div [] [ makeChart False ]
            , div [] [ makeChart True ]
            ]
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
