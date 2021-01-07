module Chart exposing
    ( DataSet
    , ReadingType(..)
    , addLayer
    , empty
    , posixToFloat
    , render
    )

import Html exposing (Html, text)
import Path
import Scale exposing (ContinuousScale)
import Shape
import Time exposing (Posix, posixToMillis)
import Tuple3
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as TypeSvgAttribute exposing (Paint(..), Transform(..))


type alias Series reading =
    { label : String
    , accessor : reading -> Maybe Float
    , fill : Paint
    , line : Paint
    , gapFill : Paint
    }


type alias IndexedSeries reading =
    { index : Int
    , series : Series reading
    }


type ReadingType reading
    = Accumulated
        { startAccessor : reading -> Float
        , endAccessor : reading -> Float
        }
    | Point
        { xAccessor : reading -> Float
        }


type alias DataSet reading =
    { readings : List reading
    , seriesList : List (Series reading)
    , readingType : ReadingType reading
    , stack : Bool
    }


type alias IndexedDataSet reading =
    { readings : List reading
    , seriesList : List (IndexedSeries reading)
    , readingType : ReadingType reading
    , stack : Bool
    }


type alias ChartConfig =
    { width : Float
    , height : Float
    , xScaleConvert : Float -> Float
    , yScaleConvert : Float -> Float
    , minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    , zeroY : Float
    }


type Chart reading
    = Chart
        { layers : List (IndexedDataSet reading)
        }


posixToFloat : Posix -> Float
posixToFloat =
    posixToMillis >> toFloat


{-| Decompose a list of tuples into a tuple of lists.

    unzip [ ( 0, True ), ( 17, False ), ( 1337, True ) ] == ( [ 0, 17, 1337 ], [ True, False, True ] )

-}
unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 triplets =
    let
        step ( x, y, z ) ( xs, ys, zs ) =
            ( x :: xs, y :: ys, z :: zs )
    in
    List.foldr step ( [], [], [] ) triplets


empty : Chart reading
empty =
    Chart
        { layers = []
        }


render :
    { size : ( Float, Float )
    , startTime : Posix
    , endTime : Posix
    }
    -> Chart reading
    -> Html msg
render options (Chart { layers }) =
    let
        ( width, height ) =
            options.size

        ( minY, maxY ) =
            layers
                |> List.foldl
                    (\{ seriesList, readings } ( dataSetMinY, dataSetMaxY ) ->
                        seriesList
                            |> List.foldl
                                (\indexedSeries ( accMinY, accMaxY ) ->
                                    let
                                        seriesAccessor =
                                            indexedSeries.series.accessor
                                    in
                                    readings
                                        |> List.foldl
                                            (\reading ( seriesMinY, seriesMaxY ) ->
                                                let
                                                    v =
                                                        seriesAccessor reading
                                                            |> Maybe.withDefault 0
                                                in
                                                ( min seriesMinY v, max seriesMaxY v )
                                            )
                                            ( accMinY, accMaxY )
                                )
                                ( dataSetMinY, dataSetMaxY )
                            |> Debug.log "( minY, maxY )"
                    )
                    ( 0, 0 )

        minX =
            posixToFloat options.startTime

        maxX =
            posixToFloat options.endTime

        xScale =
            Scale.linear ( 0, width ) ( minX, maxX )

        yScale =
            Scale.linear ( 0, height ) ( minY, maxY )

        zeroY =
            Scale.convert yScale 0

        chartConfig : ChartConfig
        chartConfig =
            { xScaleConvert = Scale.convert xScale
            , yScaleConvert = Scale.convert yScale
            , minX = minX
            , maxX = maxX
            , minY = minY
            , maxY = maxX
            , zeroY = zeroY
            , width = width
            , height = height
            }
    in
    svg
        [ viewBox 0 0 width height ]
        [ g
            [ class [ "series" ]
            , transform
                [ Translate 0 height
                , TypeSvgAttribute.Scale 1 -1
                ]
            ]
            (layers
                |> List.map (renderDataSet chartConfig)
            )
        ]


addLayer : DataSet reading -> Chart reading -> Chart reading
addLayer dataSet (Chart { layers }) =
    Chart
        { layers =
            List.append layers
                [ { readings = dataSet.readings
                  , seriesList =
                        dataSet.seriesList
                            |> List.indexedMap
                                (\i series ->
                                    { series = series
                                    , index = i
                                    }
                                )
                  , readingType = dataSet.readingType
                  , stack = dataSet.stack
                  }
                ]
        }


renderDataSet : ChartConfig -> IndexedDataSet reading -> Svg msg
renderDataSet chartConfig { readings, seriesList, readingType, stack } =
    let
        { xScaleConvert, yScaleConvert } =
            chartConfig

        dataSetRenderer : IndexedSeries reading -> List (Svg msg)
        dataSetRenderer indexedSeries =
            case readingType of
                Accumulated { startAccessor, endAccessor } ->
                    let
                        startAccessorScale =
                            startAccessor >> xScaleConvert

                        endAccessorScale =
                            endAccessor >> xScaleConvert

                        readingsMapped =
                            readings
                                |> List.map
                                    (\reading ->
                                        ( ( startAccessorScale reading, endAccessorScale reading )
                                        , indexedSeries.series.accessor reading
                                            |> Maybe.map (\y_ -> ( yScaleConvert 0, yScaleConvert y_ ))
                                        )
                                    )
                    in
                    readingsMapped
                        |> List.map (renderAccumulatedSeries chartConfig indexedSeries)

                Point { xAccessor } ->
                    let
                        xAccessorScale =
                            xAccessor >> xScaleConvert

                        readingsMapped =
                            readings
                                |> List.map
                                    (\reading ->
                                        ( xAccessorScale reading
                                        , indexedSeries.series.accessor reading
                                            |> Maybe.map (\y_ -> ( yScaleConvert 0, yScaleConvert y_ ))
                                        )
                                    )
                    in
                    readingsMapped
                        |> renderPointSeries chartConfig indexedSeries

        stackedDataSetRenderer : List (Svg msg)
        stackedDataSetRenderer =
            case readingType of
                Accumulated { startAccessor, endAccessor } ->
                    let
                        startAccessorScale =
                            startAccessor >> xScaleConvert

                        endAccessorScale =
                            endAccessor >> xScaleConvert

                        ( readingsMapped, newMinY, newMaxY ) =
                            readings
                                |> List.map
                                    (\reading ->
                                        seriesList
                                            |> List.foldl
                                                (\indexedSeries ( allValues, minY, maxY ) ->
                                                    let
                                                        value =
                                                            indexedSeries.series.accessor reading
                                                                |> Maybe.withDefault 0
                                                                |> yScaleConvert
                                                                |> Debug.log "value"

                                                        ( y0, y1 ) =
                                                            (if value > 0 then
                                                                ( maxY, value + maxY )

                                                             else
                                                                ( value + minY, minY )
                                                            )
                                                                |> Debug.log "( y0, y1 )"

                                                        _ =
                                                            ( min minY y0
                                                            , max maxY y1
                                                            )
                                                                |> Debug.log "( newMinY, newMaxY )"
                                                    in
                                                    ( ( indexedSeries
                                                      , ( ( startAccessorScale reading, endAccessorScale reading )
                                                        , Just ( y0, y1 )
                                                        )
                                                      )
                                                        :: allValues
                                                    , min minY y0
                                                    , max maxY y1
                                                    )
                                                        |> Debug.log "asdf"
                                                )
                                                ( [], 0, 0 )
                                    )
                                |> unzip3
                                |> Tuple3.mapFirst List.concat
                                |> Tuple3.mapSecond List.minimum
                                |> Tuple3.mapThird List.maximum
                                |> Debug.log "FINAL ( readingsMapped, newMinY, newMaxY )"
                    in
                    readingsMapped
                        |> List.map
                            (\( indexedSeries, reading ) ->
                                renderAccumulatedSeries chartConfig indexedSeries reading
                            )

                Point { xAccessor } ->
                    let
                        xAccessorScale =
                            xAccessor >> xScaleConvert

                        ( readingsMapped, newMinY, newMaxY ) =
                            readings
                                |> List.map
                                    (\reading ->
                                        seriesList
                                            |> List.foldl
                                                (\indexedSeries ( allValues, minY, maxY ) ->
                                                    let
                                                        _ =
                                                            ( minY, maxY )

                                                        value =
                                                            indexedSeries.series.accessor reading
                                                                |> Maybe.withDefault 0
                                                                |> yScaleConvert

                                                        ( y0, y1 ) =
                                                            if value > 0 then
                                                                ( maxY, value + maxY )

                                                            else
                                                                ( value + minY, minY )
                                                    in
                                                    ( ( indexedSeries
                                                      , ( xAccessorScale reading
                                                        , Just ( y0, y1 )
                                                            |> Debug.log (String.fromInt indexedSeries.index ++ "( y0, y1 )")
                                                        )
                                                      )
                                                        :: allValues
                                                    , min minY y0
                                                    , max maxY y1
                                                    )
                                                )
                                                ( [], 0, 0 )
                                    )
                                |> unzip3
                                |> Tuple3.mapFirst List.concat
                                |> Tuple3.mapSecond List.minimum
                                |> Tuple3.mapThird List.maximum
                                |> Debug.log "FINAL ( readingsMapped, newMinY, newMaxY )"
                    in
                    seriesList
                        |> List.concatMap
                            (\indexedSeries ->
                                let
                                    readingsFiltered =
                                        readingsMapped
                                            |> List.filterMap
                                                (\( s, p ) ->
                                                    if s.index == indexedSeries.index then
                                                        Just p

                                                    else
                                                        Nothing
                                                )
                                in
                                renderPointSeries chartConfig indexedSeries readingsFiltered
                            )
    in
    if not stack then
        seriesList
            |> List.concatMap dataSetRenderer
            |> g [ class [ "dataset" ] ]

    else
        stackedDataSetRenderer
            |> g [ class [ "dataset" ] ]


renderAccumulatedSeries :
    ChartConfig
    -> IndexedSeries reading
    -> ( ( Float, Float ), Maybe ( Float, Float ) )
    -> Svg msg
renderAccumulatedSeries chartConfig indexedSeries ( ( x0, x1 ), readingValue ) =
    let
        series =
            indexedSeries.series

        { zeroY } =
            chartConfig
    in
    case Debug.log "readingValue" readingValue of
        Just ( y0, y1 ) ->
            let
                ( y_, h ) =
                    (if y1 < y0 then
                        ( y1, y0 )

                     else
                        ( y0, y1 )
                    )
                        |> Debug.log "( y_, h )"
            in
            rect
                [ class [ "series-" ++ series.label ]
                , fill <| series.fill
                , stroke <| series.fill
                , strokeWidth 0.35
                , x x0
                , y y_
                , width (x1 - x0)
                , height (h - y_)
                ]
                []

        Nothing ->
            let
                { minY, maxY } =
                    chartConfig
            in
            rect
                [ class [ "series-" ++ series.label ]
                , fill series.gapFill
                , stroke series.gapFill
                , strokeWidth 0.35
                , x x0
                , y minY
                , width (x1 - x0)
                , height (maxY - minY)
                ]
                []


renderPointSeries :
    ChartConfig
    -> IndexedSeries reading
    -> List ( Float, Maybe ( Float, Float ) )
    -> List (Svg msg)
renderPointSeries chartConfig indexedSeries readings =
    let
        { zeroY } =
            chartConfig

        shape =
            Shape.monotoneInXCurve
    in
    [ readings
        |> List.map
            (\( x, v ) ->
                v
                    |> Debug.log ("v" ++ String.fromInt indexedSeries.index)
                    |> Maybe.map
                        (\( _, y1 ) ->
                            ( x
                            , y1
                            )
                        )
            )
        |> Shape.line shape
        |> (\path ->
                Path.element path
                    [ stroke indexedSeries.series.line
                    , strokeWidth 2
                    , fill PaintNone
                    ]
           )
    , readings
        |> List.map
            (\( x, v ) ->
                v
                    |> Maybe.map
                        (\( y0, y1 ) ->
                            if y0 > y1 then
                                ( ( x, y0 )
                                , ( x, y1 )
                                )

                            else
                                ( ( x, y0 )
                                , ( x, y1 )
                                )
                        )
            )
        |> Shape.area shape
        |> (\path ->
                Path.element path
                    [ strokeWidth 2
                    , fill indexedSeries.series.fill
                    ]
           )
    ]
