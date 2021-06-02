module Main exposing (..)

import Browser
import Chart exposing (AccumulatedLayers, Element(..), PointLayer(..))
import Chart.Elements as Elements
import Chart.Elements.XAxis as X
import Chart.Elements.YAxis as YAxis
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, style)
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
                  , end = 100000000000
                  , a = 1234
                  , b = Just 22
                  }
                , { start = 100000000000
                  , end = 110000000000
                  , a = 44
                  , b = Nothing
                  }
                , { start = 110000000000
                  , end = 200000000000
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
                [ { time = 5000000000
                  , a = -123
                  , b = Just 100
                  }
                , { time = 40000000000
                  , a = 4
                  , b = Just 2890
                  }
                , { time = 80000000000
                  , a = 420
                  , b = Just 2000
                  }
                , { time = 120000000000
                  , a = 234
                  , b = Nothing
                  }
                , { time = 150000000000
                  , a = 534
                  , b = Just 100
                  }
                , { time = 175000000000
                  , a = 534
                  , b = Just 80
                  }
                , { time = 200000000000
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
            (Elements.yAxis
                { placement = YAxis.Inside
                , position = YAxis.Right
                , tickFormatter = \chartConfig -> String.fromFloat
                }
            )
        |> Chart.add
            (Elements.xAxis
                { tickFormatter = always String.fromFloat
                , paddingLeft = 40
                , paddingBottom = 40
                }
            )
        |> Chart.render
            { size = ( 1000, 400 )
            , start =  0
            , end =  200000000000
            , yForZero = 1000
            , timeZone = Time.utc
            , attributes =  [attribute "test-id" "test"]
            }

makeZeroesChart :  Html Msg
makeZeroesChart  =
    Chart.empty
        |> Chart.addDataSet
            { readings =
                [ { start = 0
                  , end = 100000000000
                  , a = 0
                  , b = Just 0
                  }
                , { start = 100000000000
                  , end = 110000000000
                  , a = 0
                  , b = Nothing
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
            , stack = False
            }
        |> Chart.add
            (Elements.yAxis
                { placement = YAxis.Inside
                , position = YAxis.Right
                , tickFormatter = \chartConfig -> String.fromFloat
                }
            )
        |> Chart.add
            (Elements.xAxis
                { tickFormatter = always String.fromFloat
                , paddingLeft = 40
                , paddingBottom = 40
                }
            )
        |> Chart.render
            { size = ( 1000, 400 )
            , start =  0
            , end =  200000000000
            , yForZero = 1000
            , timeZone = Time.utc
            , attributes =  [attribute "test-id" "test"]
            }


view : Model -> Html Msg
view model =
    div []
        [ Html.node "style" [] [ text """
        :root {
            --border: red;
            --background: orange;
            --text: black;
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
            , div [] [ makeZeroesChart ]
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
