module Chart exposing
    ( rawSpec
    , layersToView, view
    )

import FactList
import Distribution as Dist
import Axis exposing (Scale)
import Util exposing (Rect, Spec, ViewDims, Transformer)
import Path exposing (Path (Path), Instruction (M, L))

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SvgA


maxTicks : Int
maxTicks = 6

-- Dimensions for the viewBox

viewDim : ViewDims
viewDim =
    { left = 50
    , right = 50
    , top = 20
    , bottom = 80
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
        , viewLines transformer scaledSpec
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

-- Render the distribution as lines.
-- We also have a function to transform and scale the chart's spec

viewLines : Transformer -> Spec -> Svg x
viewLines transformer spec =
    let
        trans x y = (transformer.trX x, transformer.trY y)
        path = distPath spec |> Path.map trans
    in
        Svg.path
        [ SvgA.d (Path.d path)
        , SvgA.stroke "red"
        , SvgA.strokeWidth "2"
        , SvgA.fill "none"
        ]
        []

distPath : Spec -> Path
distPath spec =
    case spec.rects of
        [] -> Path []
        rect :: tail ->
            Path (distPath' spec.rects [M (rect.left) 0])

distPath' : List Rect -> List Instruction -> List Instruction
distPath' rects accum =
    case rects of
        [] ->
            accum
        rect :: tail ->
            let
                midPoint = (rect.left + rect.right) / 2
                accum' = L midPoint rect.height :: accum
            in
                if (List.isEmpty tail) then
                    L rect.right 0 :: accum'
                        |> List.reverse
                else
                    distPath' tail accum'

