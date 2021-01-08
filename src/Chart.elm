module Chart exposing
    ( DataSet
    , ReadingType(..)
    , addDataSet
    , empty
    , posixToFloat
    , render
    )

import Html exposing (Html, text)
import List.Extra
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



-- CONSTANTS


lineWidth : Float
lineWidth =
    2



-- TYPES


type alias Series reading =
    { label : String
    , accessor : reading -> Maybe Float
    , fill : Paint
    , line : Paint
    , gapFill : Paint
    }


type alias InternalSeries =
    { index : Int
    , label : String
    , fill : Paint
    , line : Paint
    , gapFill : Paint
    }


type alias InternalDatum =
    --
    ( ( Float -- x
      , Float -- width
      )
    , Maybe
        ( Float -- y
        , Float -- height
        , Float -- extend = y + height
        )
    )


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


type alias InternalDataSet =
    { readingsList : List (List InternalDatum)
    , seriesList : List InternalSeries
    , readingType : ReadingType InternalDatum
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


type Chart
    = Chart
        { layers : List InternalDataSet
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


empty : Chart
empty =
    Chart
        { layers = []
        }


addDataSet : DataSet reading -> Chart -> Chart
addDataSet dataSet (Chart { layers }) =
    Chart
        { layers =
            List.append layers
                [ dataSetMapper dataSet
                ]
        }


dataSetMapper : DataSet reading -> InternalDataSet
dataSetMapper dataSet =
    let
        { readingType, readings, seriesList, stack } =
            dataSet

        newReadings : List (List InternalDatum)
        newReadings =
            case readingType of
                Accumulated { startAccessor, endAccessor } ->
                    let
                        readingsMapped : List (List InternalDatum)
                        readingsMapped =
                            readings
                                |> List.map
                                    (\reading ->
                                        seriesList
                                            |> List.foldl
                                                (\series ( allValues, minY, maxY ) ->
                                                    let
                                                        value =
                                                            series.accessor reading
                                                                |> Maybe.withDefault 0

                                                        ( y0, y1, extend ) =
                                                            if stack then
                                                                if value > 0 then
                                                                    ( maxY, value + maxY, value + maxY )

                                                                else
                                                                    ( minY, value + minY, value + minY )

                                                            else
                                                                ( 0, value, value )
                                                    in
                                                    ( ( ( startAccessor reading, endAccessor reading )
                                                      , Just ( y0, y1, extend )
                                                      )
                                                        :: allValues
                                                    , min minY y0
                                                    , max maxY y1
                                                    )
                                                )
                                                ( [], 0, 0 )
                                    )
                                |> unzip3
                                |> Tuple3.first
                    in
                    readingsMapped
                        |> List.Extra.transpose

                Point { xAccessor } ->
                    let
                        readingsMapped : List (List InternalDatum)
                        readingsMapped =
                            readings
                                |> List.map
                                    (\reading ->
                                        seriesList
                                            |> List.foldl
                                                (\series ( allValues, minY, maxY ) ->
                                                    let
                                                        value =
                                                            series.accessor reading
                                                                |> Maybe.withDefault 0

                                                        ( y0, y1, extend ) =
                                                            if stack then
                                                                if value > 0 then
                                                                    ( maxY, value + maxY, value + maxY )

                                                                else
                                                                    ( minY, value + minY, value + minY )

                                                            else
                                                                ( 0, value, value )
                                                    in
                                                    ( ( ( xAccessor reading
                                                        , 0
                                                        )
                                                      , Just ( y0, y1, extend )
                                                      )
                                                        :: allValues
                                                    , min minY y0
                                                    , max maxY y1
                                                    )
                                                )
                                                ( [], 0, 0 )
                                    )
                                |> unzip3
                                |> Tuple3.first
                    in
                    readingsMapped
                        |> List.Extra.transpose
    in
    { readingsList = newReadings
    , seriesList =
        seriesList
            |> List.indexedMap seriesToInteral
    , readingType =
        case readingType of
            Accumulated _ ->
                Accumulated
                    { startAccessor = Tuple.first >> Tuple.first
                    , endAccessor = Tuple.first >> Tuple.second
                    }

            Point _ ->
                Point { xAccessor = Tuple.first >> Tuple.first }
    , stack = stack
    }


seriesToInteral : Int -> Series reading -> InternalSeries
seriesToInteral index series =
    { index = index
    , label = series.label
    , fill = series.fill
    , line = series.line
    , gapFill = series.gapFill
    }


render :
    { size : ( Float, Float )
    , startTime : Posix
    , endTime : Posix
    }
    -> Chart
    -> Html msg
render options (Chart { layers }) =
    let
        ( width, height ) =
            options.size

        verticalPadding =
            lineWidth

        horizontalPadding =
            lineWidth

        -- for the stroke
        ( minY, maxY ) =
            layers
                |> List.foldl
                    (\{ readingsList } ( dataSetMinY, dataSetMaxY ) ->
                        readingsList
                            |> List.foldl
                                (\readings ( accMinY, accMaxY ) ->
                                    let
                                        extendAccessor =
                                            Tuple.second >> Maybe.map Tuple3.third
                                    in
                                    readings
                                        |> List.foldl
                                            (\reading ( seriesMinY, seriesMaxY ) ->
                                                let
                                                    v =
                                                        extendAccessor reading
                                                            |> Maybe.withDefault 0
                                                in
                                                ( min seriesMinY v, max seriesMaxY v )
                                            )
                                            ( accMinY, accMaxY )
                                        |> Debug.log "( accMinY, accMaxY )"
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
            Scale.linear ( 0, width - horizontalPadding ) ( minX, maxX )

        yScale =
            Scale.linear ( 0, height - verticalPadding ) ( minY, maxY )

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
                [ Translate (0 + horizontalPadding / 2) (height - verticalPadding / 2)
                , TypeSvgAttribute.Scale 1 -1
                ]
            ]
            (layers
                |> List.concatMap
                    (\{ readingsList, seriesList, readingType, stack } ->
                        List.map2 (renderDataSet chartConfig readingType stack) seriesList readingsList
                    )
            )
        ]


renderDataSet : ChartConfig -> ReadingType InternalDatum -> Bool -> InternalSeries -> List InternalDatum -> Svg msg
renderDataSet chartConfig readingType stack internalSeries readings =
    let
        { xScaleConvert, yScaleConvert } =
            chartConfig

        dataSetRenderer : List (Svg msg)
        dataSetRenderer =
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
                                        , reading
                                            |> Tuple.second
                                            |> Maybe.map
                                                (\( y0, y1, extend ) ->
                                                    ( yScaleConvert y0, yScaleConvert y1, 0 )
                                                )
                                        )
                                    )
                    in
                    readingsMapped
                        |> List.map (renderAccumulatedSeries chartConfig internalSeries)

                Point { xAccessor } ->
                    let
                        xAccessorScale =
                            xAccessor >> xScaleConvert

                        readingsMapped =
                            readings
                                |> List.map
                                    (\reading ->
                                        let
                                            x =
                                                xAccessorScale reading
                                        in
                                        ( ( x, x )
                                        , reading
                                            |> Tuple.second
                                            |> Maybe.map
                                                (\( y0, y1, extend ) ->
                                                    ( yScaleConvert y0, yScaleConvert y1, 0 )
                                                )
                                        )
                                    )
                    in
                    readingsMapped
                        |> renderPointSeries chartConfig internalSeries
    in
    dataSetRenderer
        |> g [ class [ "dataset" ] ]


renderAccumulatedSeries :
    ChartConfig
    -> InternalSeries
    -> InternalDatum
    -> Svg msg
renderAccumulatedSeries chartConfig internalSeries ( ( x0, x1 ), readingValue ) =
    let
        { zeroY } =
            chartConfig
    in
    case Debug.log "readingValue" readingValue of
        Just ( y0, y1, _ ) ->
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
                [ class [ "series-" ++ internalSeries.label ]
                , fill <| internalSeries.fill
                , stroke <| internalSeries.fill
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
                [ class [ "series-" ++ internalSeries.label ]
                , fill internalSeries.gapFill
                , stroke internalSeries.gapFill
                , strokeWidth 0.35
                , x x0
                , y minY
                , width (x1 - x0)
                , height (maxY - minY)
                ]
                []


renderPointSeries :
    ChartConfig
    -> InternalSeries
    -> List InternalDatum
    -> List (Svg msg)
renderPointSeries chartConfig internalSeries readings =
    let
        { zeroY } =
            chartConfig

        shape =
            Shape.monotoneInXCurve
    in
    [ readings
        |> List.map
            (\( ( x, _ ), v ) ->
                v
                    |> Maybe.map
                        (\( y0, y1, _ ) ->
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
                    [ strokeWidth lineWidth
                    , fill internalSeries.fill
                    ]
           )
    , readings
        |> List.map
            (\( ( x, _ ), v ) ->
                v
                    |> Debug.log ("v" ++ String.fromInt internalSeries.index)
                    |> Maybe.map
                        (\( _, y1, _ ) ->
                            ( x
                            , y1
                            )
                        )
            )
        |> Shape.line shape
        |> (\path ->
                Path.element path
                    [ stroke internalSeries.line
                    , strokeWidth lineWidth
                    , fill PaintNone
                    ]
           )
    ]
