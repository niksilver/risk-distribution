module Chart exposing (view)

import FactList
import Distribution as Dist
import Axis exposing (Scale)
import Spec exposing (Spec, ViewDims, Transformer)
import Block exposing (Rect)
import Value exposing (Value (Exactly, Maximum, Contradiction))
import Path exposing (Path (Path), Instruction (M, L))
import Spline exposing (Pos)
import Util

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

-- View

view : Spec -> Html x
view spec =
    let
        -- Get the distribution curve and its min and max points

        curve = distCurve spec
        (curveMin, curveMax) =
            Spline.yMinMax curve
                |> Maybe.withDefault (0, spec.maxY)

        -- Rescale the chart spec to include an x-axis with nice max and min
        -- and a curve that might go higher than the tallest rect

        scale = Axis.scale spec.minX spec.maxX maxTicks
        scaledSpec =
            { spec
            | minX = scale.min
            , maxX = scale.max
            , maxY = max spec.maxY curveMax
            }
        transformer = Spec.transformer viewDim scaledSpec

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
        [ viewBlocks transformer scaledSpec
        , viewCurve transformer curve
        , Axis.viewXAxis transformer scale
        ]

-- Render just the distribution area as blocks,
-- given functions to transform and scale a given spec

viewBlocks : Transformer -> Spec -> Svg x
viewBlocks transformer spec =
    let
        draw chBlock =
            case chBlock.value of
                Exactly _ _ ->
                    drawExactly transformer chBlock.rect
                _ ->
                    Svg.svg [] []
    in
        Svg.g []
        (List.map draw spec.blocks)

drawExactly transformer rect =
    Svg.rect
    [ SvgA.x (rect.left |> transformer.trX |> toString)
    , SvgA.y (rect.height |> transformer.trY |> toString)
    , SvgA.width (rect.right - rect.left |> transformer.scX |> toString)
    , SvgA.height (rect.height |> transformer.scY |> toString)
    , SvgA.fill "rgb(82, 92, 227)"
    ]
    []

-- Render the distribution as a curve,
-- given functions to transform and the curve itself

viewCurve : Transformer -> List Pos -> Svg x
viewCurve transformer curve =
    let
        trans x y =
            Pos (transformer.trX x) (transformer.trY y)
        path =
            curve
                |> Path.fromPosList
                |> Path.map trans
    in
        Svg.path
        [ SvgA.d (Path.d path)
        , SvgA.stroke "purple"
        , SvgA.strokeWidth "2"
        , SvgA.fill "rgba(124, 60, 155, 0.6)"
        ]
        []

-- Create a distribution curve

distCurve : Spec -> List Pos
distCurve spec =
    let
        rects = Spec.rects spec

        -- To create the path for distribution curve we take
        -- the initial set of rectangles in the chart spec and...
        -- Put pretend rectangles at the start and end to
        -- get a sense of where the curve should start and end from;
        -- Take the rectangles in sliding groups of three and see
        -- If the middle one is at a peak, in a dip, etc and add
        -- curve points accordingly;
        -- Join up the points with a spline.
        -- Squash the curve up if it falls below the x-axis
    in
        rects
            |> Spec.bracketRects
            |> Util.sliding 3
            |> List.map curvePoints
            |> List.concat
            |> addEndsOfSpline rects
            |> Spline.splines 20
            |> squash

-- Translate to Spec.curvePointsForRect, but taking a list
-- of three rectangles instead of three separate Rect arguments

curvePoints : List Rect -> List Pos
curvePoints rects =
    case rects of
        prev :: rect :: next :: [] ->
            Spec.curvePointsForRect prev rect next
        _ ->
            []

-- Squash the curve up if it's below the x-axis

squash : List Pos -> List Pos
squash ps =
    let
        trans p =
            Pos p.x (max p.y 0)
    in
        List.map trans ps

-- Add end points to the distribution curve

addEndsOfSpline : List Rect -> List Pos -> List Pos
addEndsOfSpline rects points =
    points
        |> addFrontOfSpline rects
        |> addBackOfSpline rects

addPosIfDifferent : Pos -> List Pos -> List Pos
addPosIfDifferent p ps =
    if (List.head ps == Just p) then
        ps
    else
        p :: ps

addFrontOfSpline : List Rect -> List Pos -> List Pos
addFrontOfSpline rects points =
    let
        pos =
            case List.head rects of
                Nothing ->
                    Pos 0 0
                Just rect ->
                    Pos rect.left 0
    in
        addPosIfDifferent pos points

addBackOfSpline : List Rect -> List Pos -> List Pos
addBackOfSpline rects points =
    let
        pos =
            case (List.reverse rects |> List.head) of
                Nothing ->
                    Pos 0 0
                Just rect ->
                    Pos rect.right 0
    in
        points
            |> List.reverse
            |> addPosIfDifferent pos
            |> List.reverse
