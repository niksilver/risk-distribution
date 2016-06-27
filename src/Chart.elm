module Chart exposing
    ( rawSpec
    , layersToView, view
    )

import FactList
import Distribution as Dist
import Axis exposing (Scale)
import Util exposing (Spec, ViewDims, Transformer)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SvgA


maxTicks : Int
maxTicks = 8

-- Dimensions for the viewBox

viewDim : ViewDims
viewDim =
    { left = 50
    , top = 20
    , width = 800
    , height = 350
    }

-- Produce a scaled and positioned spec for the chart

-- First, a mechanism to get an unscaled spec.
-- The area of each rectangle will be its probability

rawSpec : List Dist.Layer -> Maybe Spec
rawSpec layers =
    let
        intervals = layers |> Dist.intervals |> Dist.sort
        int2Rect int =
            { left = int.lower
            , right = int.upper
            , height = int.prob / (int.upper - int.lower)
            }
        rects = List.map int2Rect intervals
        maxHeight = List.map .height rects |> List.maximum
        toSpec (minX, maxX) maxY =
            { minX = minX, maxX = maxX, maxY = maxY, rects = rects }
    in
        Maybe.map2
            toSpec
            (Dist.range intervals)
            maxHeight

-- View

-- Shortcut

type alias FloatFn = Float -> Float

layersToView : List Dist.Layer -> Html x
layersToView layers =
    case (rawSpec layers) of
        Just spec -> view spec
        Nothing -> Svg.text ""
 
view : Spec -> Html x
view spec =
    let
        scale = Axis.scale spec.minX spec.maxX maxTicks
        scaledSpec =
            { spec
            | minX = scale.min
            , maxX = scale.max
            }
        trX = Util.transformX viewDim scaledSpec
        trY = Util.transformY viewDim scaledSpec
        scX = Util.scaleX viewDim scaledSpec
        scY = Util.scaleY viewDim scaledSpec
        viewBoxDim =
            "0 0 "
            ++ (2 * viewDim.left + viewDim.width |> toString) ++ " "
            ++ (2 * viewDim.top + viewDim.height |> toString)
    in
        Svg.svg
        [ SvgA.width "100%"
        , SvgA.height "400px"
        , SvgA.viewBox viewBoxDim
        ]
        [ viewDist trX trY scX scY scaledSpec
        , viewXAxis trX trY scX scY scale
        ]

-- Render just the distribution area given functions to transform and
-- scale a given spec

viewDist : FloatFn -> FloatFn -> FloatFn -> FloatFn -> Spec -> Svg x
viewDist trX trY scX scY spec =
    let
        draw rect =
            Svg.rect
            [ SvgA.x (rect.left |> trX |> toString)
            , SvgA.y (rect.height |> trY |> toString)
            , SvgA.width (rect.right - rect.left |> scX |> toString)
            , SvgA.height (rect.height |> scY |> toString)
            , SvgA.fill "blue"
            ]
            []
    in
        Svg.g []
        (List.map draw spec.rects)

-- Render the x-axis given functions to transform and scale the scale

viewXAxis : FloatFn -> FloatFn -> FloatFn -> FloatFn -> Scale -> Svg x
viewXAxis trX trY scX scY scale =
    let
        y = trY 0
        trScale =
            { scale
            | min = trX scale.min
            , max = trX scale.max
            , step = scX scale.step
            }
    in
        Axis.viewXAxis y trScale

