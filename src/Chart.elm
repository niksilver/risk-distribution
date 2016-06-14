module Chart exposing
    ( Spec, ViewDims
    , rawSpec
    , transformX, transformY
    , layersToView, view
    )

import FactList
import Distribution as Dist

import Html exposing (Html)
import Svg exposing
    ( Svg
    , svg, text
    )
import Svg.Attributes exposing (width, height, x, y, viewBox)


-- Specification for a chart

type alias Spec =
    { minX : Float
    , maxX : Float
    , maxY : Float
    , rects : List { left : Float, right : Float, height : Float }
    }

-- Dimensions for a viewBox

type alias ViewDims =
    { left : Float
    , top : Float
    , width : Float
    , height : Float
    }

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

-- Transform a point on the x-axis or y-axis from its place in a spec
-- to a place in the chart view box.

transformX : ViewDims -> Spec -> Float -> Float
transformX dims spec x =
    dims.left + (x - spec.minX) / (spec.maxX - spec.minX) * dims.width

transformY : ViewDims -> Spec -> Float -> Float
transformY dims spec y =
    dims.top + (spec.maxY - y) / spec.maxY * dims.height


-- View

layersToView : List Dist.Layer -> Html x
layersToView layers =
    case (rawSpec layers) of
        Just spec -> view spec
        Nothing -> text ""
 
view : Spec -> Html x
view spec =
    let
        viewBoxDim =
            "0 0 "
            ++ (2 * viewDim.left + viewDim.width |> toString) ++ " "
            ++ (2 * viewDim.top + viewDim.height |> toString)
    in
        svg
        [ width "100%"
        , height "400px"
        , viewBox viewBoxDim
        ]
        (viewArea spec)

-- Render just the distribution area

viewArea : Spec -> List (Svg x)
viewArea spec =
    let
        trX = transformX viewDim spec
        trY = transformY viewDim spec
        draw rect =
            Svg.rect
            [ x (rect.left |> trX |> toString)
            , y (rect.height |> trY |> toString)
            , width (rect.right - rect.left |> trX |> toString)
            , height (0 |> trY |> toString)
            ]
            []
    in
        List.map draw spec.rects

