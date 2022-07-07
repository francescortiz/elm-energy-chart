module EChart exposing
    ( AccumulatedLayers
    , DataSet
    , Element(..)
    , PointLayer(..)
    , ReadingType(..)
    , Series
    , add
    , addDataSet
    , empty
    , posixToFloat
    , render
    )

import EChart.Types exposing (ChartConfig, ChartTick, ElementDefinition, InternalDatum, Padding)
import Html exposing (Attribute, Html, text)
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape
import SpringDesign.Utils exposing (floatToPosix)
import SubPath exposing (SubPath)
import Svg.Attributes as Attributes exposing (fill, id, preserveAspectRatio, stroke)
import Time exposing (Posix, millisToPosix, posixToMillis)
import Tuple3
import TypedSvg exposing (clipPath, defs, g, rect, svg)
import TypedSvg.Attributes exposing (class, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as TypeSvgAttribute exposing (Transform(..))



-- CONSTANTS


defaultLineWidth : Float
defaultLineWidth =
    2



-- TYPES


type alias Series reading =
    { label : String
    , accessor : reading -> Maybe Float
    , fill : String
    , line : String
    , gapFill : String
    }


type alias InternalSeries =
    { index : Int
    , label : String
    , fill : String
    , line : String
    , gapFill : String
    }


type alias AccumulatedLayers =
    { solid : Bool
    , gaps : Bool
    }


type PointLayer
    = LineLayer
    | FillLayer


type ReadingType reading
    = Accumulated
        { startAccessor : reading -> Float
        , endAccessor : reading -> Float
        , layers : AccumulatedLayers
        , barPadding : Float
        }
    | Point
        { xAccessor : reading -> Float
        , layers : List PointLayer
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


type Element msg
    = DataSetElement InternalDataSet
    | Element (ElementDefinition msg)


type Chart msg
    = Chart
        { elements : List (Element msg)
        }



-- HELPERS


isDataSetElement : Element msg -> Maybe InternalDataSet
isDataSetElement element =
    case element of
        DataSetElement internalDataset ->
            Just internalDataset

        _ ->
            Nothing


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


empty : Chart msg
empty =
    Chart
        { elements = []
        }


addDataSet : DataSet reading -> Chart msg -> Chart msg
addDataSet dataSet (Chart chart) =
    Chart
        { chart
            | elements =
                DataSetElement (dataSetMapper dataSet) :: chart.elements
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
                                                        value_ =
                                                            series.accessor reading

                                                        ( mappedValue, y0, y1 ) =
                                                            case value_ of
                                                                Just value ->
                                                                    let
                                                                        ( y0_, y1_, extend ) =
                                                                            if stack then
                                                                                if value > 0 then
                                                                                    ( maxY, value + maxY, value + maxY )

                                                                                else
                                                                                    ( minY, value + minY, value + minY )

                                                                            else
                                                                                ( 0, value, value )
                                                                    in
                                                                    ( Just ( y0_, y1_, extend ), y0_, y1_ )

                                                                Nothing ->
                                                                    ( Nothing, 0, 0 )
                                                    in
                                                    ( ( ( startAccessor reading, endAccessor reading )
                                                      , mappedValue
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
                                                        ( mappedValue, y0, y1 ) =
                                                            case series.accessor reading of
                                                                Just value ->
                                                                    let
                                                                        ( y0_, y1_, extent ) =
                                                                            if stack then
                                                                                if value > 0 then
                                                                                    ( maxY, value + maxY, value + maxY )

                                                                                else
                                                                                    ( minY, value + minY, value + minY )

                                                                            else
                                                                                ( 0, value, value )
                                                                    in
                                                                    ( Just ( y0_, y1_, extent ), y0_, y1_ )

                                                                Nothing ->
                                                                    ( Nothing, 0, 0 )
                                                    in
                                                    ( ( ( xAccessor reading
                                                        , 0
                                                        )
                                                      , mappedValue
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
            |> List.reverse
    , readingType =
        case readingType of
            Accumulated { layers, barPadding } ->
                Accumulated
                    { startAccessor = Tuple.first >> Tuple.first
                    , endAccessor = Tuple.first >> Tuple.second
                    , layers = layers
                    , barPadding = barPadding
                    }

            Point { layers } ->
                Point
                    { xAccessor = Tuple.first >> Tuple.first
                    , layers = layers
                    }
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


add : ElementDefinition msg -> Chart msg -> Chart msg
add element (Chart chart) =
    Chart
        { chart
            | elements = Element element :: chart.elements
        }


contributeElementToPadding : Element msg -> Padding
contributeElementToPadding element =
    case element of
        DataSetElement _ ->
            Padding 0 0 0 0

        Element { contributeToPadding } ->
            contributeToPadding


contributeElementToMaxXTicks : Float -> Element msg -> Maybe Int
contributeElementToMaxXTicks heightInPx element =
    case element of
        DataSetElement _ ->
            Nothing

        Element { contributeToMaxXTicks } ->
            contributeToMaxXTicks heightInPx


contributeElementToMaxYTicks : Float -> Element msg -> Maybe Int
contributeElementToMaxYTicks heightInPx element =
    case element of
        DataSetElement _ ->
            Nothing

        Element { contributeToMaxYTicks } ->
            contributeToMaxYTicks heightInPx


maxPaddings : List Padding -> Padding
maxPaddings paddings =
    paddings
        |> List.foldr
            (\accPadding { top, right, bottom, left } ->
                { top = max accPadding.top top
                , right = max accPadding.right right
                , bottom = max accPadding.bottom bottom
                , left = max accPadding.left left
                }
            )
            (Padding 0 0 0 0)


render :
    { size : ( Float, Float )
    , start : Float
    , end : Float
    , yForZero : Float
    , timeZone : Time.Zone
    , attributes : List (Attribute msg)
    }
    -> Chart msg
    -> Html msg
render { size, start, end, yForZero, timeZone, attributes } (Chart { elements }) =
    let
        ( chartWidth, chartHeight ) =
            size

        halfLineWidth =
            defaultLineWidth / 2

        padding =
            elements
                |> List.map contributeElementToPadding
                |> List.append [ Padding halfLineWidth halfLineWidth halfLineWidth halfLineWidth ]
                |> maxPaddings

        canvasHeight =
            chartHeight - padding.top - padding.bottom

        maxXTicks =
            elements
                |> List.filterMap (contributeElementToMaxXTicks canvasHeight)
                |> List.minimum

        maxYTicks =
            elements
                |> List.filterMap (contributeElementToMaxYTicks canvasHeight)
                |> List.minimum

        dataSetElements =
            elements
                |> List.filterMap isDataSetElement

        -- for the stroke
        ( dirtyMinY, dirtyMaxY_ ) =
            dataSetElements
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
                                )
                                ( dataSetMinY, dataSetMaxY )
                    )
                    ( 0, 0 )

        dirtyMaxY =
            if dirtyMinY == dirtyMaxY_ then
                dirtyMinY + yForZero

            else
                dirtyMaxY_

        yScaleNoTics =
            Scale.linear ( 0, canvasHeight ) ( dirtyMinY, dirtyMaxY )

        ( yScale, yTicksScaled, yScaleConvert ) =
            case maxYTicks of
                Just yTickCount ->
                    let
                        yScale_ =
                            yScaleNoTics
                                |> Scale.nice yTickCount

                        yScaleConvert_ =
                            Scale.convert yScale_
                    in
                    ( yScale_
                    , Scale.ticks yScale_ yTickCount
                        |> List.map
                            (\tickValue ->
                                { tickValue = tickValue
                                , tickPosition = yScaleConvert_ tickValue
                                }
                            )
                    , yScaleConvert_
                    )

                Nothing ->
                    ( yScaleNoTics
                    , []
                    , Scale.convert yScaleNoTics
                    )

        ( xScale, xTicksScaled, xScaleConvert ) =
            case maxXTicks of
                Just xTickCount ->
                    let
                        timeScale =
                            Scale.time timeZone ( 0, chartWidth - padding.left - padding.right ) ( millisToPosix (floor start), millisToPosix (floor end) )

                        -- We want to work with floats, not posix.
                        xScale_ =
                            Scale.linear
                                (Scale.range timeScale)
                                (Scale.domain timeScale
                                    |> Tuple.mapBoth posixToFloat posixToFloat
                                )

                        xScaleConvert_ =
                            Scale.convert xScale_
                    in
                    ( xScale_
                    , Scale.ticks timeScale xTickCount
                        |> List.map
                            (\timeTickValue ->
                                let
                                    tickValue =
                                        posixToFloat timeTickValue
                                in
                                { tickValue = tickValue
                                , tickPosition = xScaleConvert_ tickValue
                                }
                            )
                    , xScaleConvert_
                    )

                Nothing ->
                    let
                        -- We want to work with floats, not posix.
                        xScale_ =
                            Scale.linear ( 0, chartWidth - padding.left - padding.right ) ( start, end )

                        xScaleConvert_ =
                            Scale.convert xScale_
                    in
                    ( xScale_
                    , []
                    , xScaleConvert_
                    )

        ( minY, maxY ) =
            Scale.domain yScale

        chartConfig : ChartConfig
        chartConfig =
            { width = chartWidth
            , height = chartHeight
            , xScale = xScale
            , yScale = yScale
            , xScaleConvert = Scale.convert xScale
            , yScaleConvert = yScaleConvert
            , minYScaled = yScaleConvert minY
            , maxYScaled = yScaleConvert maxY
            , xTicks = xTicksScaled
            , yTicks = yTicksScaled
            , padding = padding
            }
    in
    svg
        ([ viewBox 0 0 chartWidth chartHeight
         , preserveAspectRatio "xMinYMin meet"
         , width chartWidth
         , height chartHeight
         ]
            ++ attributes
        )
        [ defs []
            [ clipPath [ id "plot-canvas-clip" ]
                [ rect
                    [ x 0
                    , y 0
                    , width (chartWidth - padding.left - padding.right)
                    , height (chartHeight - padding.top - padding.bottom)
                    ]
                    []
                ]
            ]
        , g
            [ class [ "chart" ]
            ]
            (elements
                |> List.reverse
                |> List.map (renderElement chartConfig)
            )
        ]


renderElement : ChartConfig -> Element msg -> Svg msg
renderElement chartConfig element =
    let
        { padding, height } =
            chartConfig
    in
    case element of
        DataSetElement internalDataSet ->
            internalDataSet
                |> renderDataSet chartConfig
                |> g
                    [ Attributes.clipPath "url(#plot-canvas-clip)"
                    , class
                        [ "dataset"
                        ]
                    , transform
                        [ Translate padding.left (height - padding.bottom)
                        , TypeSvgAttribute.Scale 1 -1
                        ]
                    ]

        Element definition ->
            definition.render chartConfig


renderDataSet : ChartConfig -> InternalDataSet -> List (Svg msg)
renderDataSet chartConfig { readingsList, seriesList, readingType, stack } =
    List.map2 (renderDataSetSeries chartConfig readingType stack) seriesList readingsList


renderDataSetSeries : ChartConfig -> ReadingType InternalDatum -> Bool -> InternalSeries -> List InternalDatum -> Svg msg
renderDataSetSeries chartConfig readingType stack internalSeries readings =
    let
        { xScaleConvert, yScaleConvert } =
            chartConfig

        dataSetRenderer : List (Svg msg)
        dataSetRenderer =
            case readingType of
                Accumulated { startAccessor, endAccessor, layers, barPadding } ->
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
                        |> List.map (renderAccumulatedSeries layers chartConfig internalSeries barPadding)

                Point { xAccessor, layers } ->
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
                        |> renderPointSeries layers chartConfig internalSeries
    in
    dataSetRenderer
        |> g [ class [ "series", "series-" ++ String.fromInt internalSeries.index ] ]


renderAccumulatedSeries :
    AccumulatedLayers
    -> ChartConfig
    -> InternalSeries
    -> Float
    -> InternalDatum
    -> Svg msg
renderAccumulatedSeries layers chartConfig internalSeries barPadding ( ( x0, x1 ), readingValue ) =
    case readingValue of
        Just ( y0__, y1__, _ ) ->
            let
                ( y0, y1 ) =
                    if y1__ < y0__ then
                        ( y1__, y0__ )

                    else
                        ( y0__, y1__ )
            in
            if layers.solid then
                renderSeriesRect internalSeries.index internalSeries.fill (x0 + barPadding) (x1 - barPadding) y0 y1

            else
                text ""

        Nothing ->
            let
                { minYScaled, maxYScaled } =
                    chartConfig
            in
            if layers.gaps then
                renderSeriesRect internalSeries.index internalSeries.gapFill x0 x1 minYScaled maxYScaled

            else
                text ""


renderSeriesRect : Int -> String -> Float -> Float -> Float -> Float -> Html msg
renderSeriesRect index fillPaint x0 x1 y0 y1 =
    rect
        [ class [ "series-" ++ String.fromInt index ]
        , fill fillPaint
        , stroke fillPaint
        , strokeWidth 0.35
        , x x0
        , y y0
        , width (x1 - x0)
        , height (y1 - y0)
        ]
        []


renderPointSeries :
    List PointLayer
    -> ChartConfig
    -> InternalSeries
    -> List InternalDatum
    -> List (Svg msg)
renderPointSeries layers chartConfig internalSeries readings =
    let
        shape =
            Shape.monotoneInXCurve

        renderedLayers =
            layers
                |> List.map (renderPointLayer shape defaultLineWidth internalSeries.line internalSeries.fill readings)
    in
    renderedLayers


renderPointLayer :
    (List ( Float, Float ) -> SubPath)
    -> Float
    -> String
    -> String
    -> List InternalDatum
    -> PointLayer
    -> Svg msg
renderPointLayer shape lineWidth linePaint fillPaint readings layer =
    case layer of
        LineLayer ->
            pointLayerLine shape lineWidth linePaint readings

        FillLayer ->
            pointLayerFill shape lineWidth fillPaint readings


pointLayerLine :
    (List ( Float, Float ) -> SubPath)
    -> Float
    -> String
    -> List InternalDatum
    -> Svg msg
pointLayerLine shape lineWidth linePaint readings =
    readings
        |> List.map
            (\( ( x, _ ), v ) ->
                v
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
                    [ stroke linePaint
                    , strokeWidth lineWidth
                    , fill "none"
                    ]
           )


pointLayerFill :
    (List ( Float, Float ) -> SubPath)
    -> Float
    -> String
    -> List InternalDatum
    -> Svg msg
pointLayerFill shape lineWidth fillPaint readings =
    readings
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
                    , fill fillPaint
                    ]
           )
