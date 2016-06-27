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
    , right = 50
    , top = 20
    , bottom = 50
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
        -- Rescale the chart spec to include an x-axis with nice max and min
        scale = Axis.scale spec.minX spec.maxX maxTicks
        scaledSpec =
            { spec
            | minX = scale.min
            , maxX = scale.max
            }
        transformer = Util.transformer viewDim scaledSpec
        viewBoxDim =
            "0 0 "
            ++ (viewDim.left + viewDim.right + viewDim.width |> toString) ++ " "
            ++ (viewDim.top + viewDim.bottom + viewDim.height |> toString)
    in
        Svg.svg
        [ SvgA.width "100%"
        , SvgA.height "400px"
        , SvgA.viewBox viewBoxDim
        ]
        [ viewDist transformer scaledSpec
        , Axis.viewXAxis transformer scale
        ]

-- Render just the distribution area given functions to transform and
-- scale a given spec

viewDist : Transformer -> Spec -> Svg x
viewDist transformer spec =
    let
        draw rect =
            Svg.rect
            [ SvgA.x (rect.left |> transformer.trX |> toString)
            , SvgA.y (rect.height |> transformer.trY |> toString)
            , SvgA.width (rect.right - rect.left |> transformer.scX |> toString)
            , SvgA.height (rect.height |> transformer.scY |> toString)
            , SvgA.fill "blue"
            ]
            []
    in
        Svg.g []
        (List.map draw spec.rects)

-- Render the x-axis given functions to transform and scale it for the view box

viewXAxis : Transformer -> Scale -> Svg x
viewXAxis transformer scale =
    let
        trScale =
            { scale
            | min = transformer.trX scale.min
            , max = transformer.trX scale.max
            , step = transformer.scX scale.step
            }
    in
        Axis.viewXAxis transformer trScale

