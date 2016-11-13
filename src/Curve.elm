module Curve exposing
    ( view, distribution
    , curvePointsForRect
    , bracketRects
    )

import Spec exposing (Spec, Transformer)
import Path exposing (Path (Path), Instruction (M, L))
import Block exposing (Rect)
import Spline exposing (Pos)
import Util

import Svg exposing (Svg)
import Svg.Attributes as SvgA

-- A curve to represent a probability distribution


-- Render the distribution as a curve,
-- given functions to transform and the curve itself

view : Transformer -> List Pos -> Svg x
view transformer curve =
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

distribution : Spec -> List Pos
distribution spec =
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
            |> bracketRects
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
            curvePointsForRect prev rect next
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

-- Given a rectangle (and the rectangles before and after it)
-- return a list of points where a distribution curve should run through.
-- If a point needs to go on the border between two neighbouring rectangles
-- then it's assumed to be set by the right hand one.

curvePointsForRect : Rect -> Rect -> Rect -> List Pos
curvePointsForRect prev rect next =
    let
        height = rect.height
        mid = (rect.left + rect.right) / 2
        midLeft  = (2 * rect.left / 3) + (rect.right / 3)
        midRight = (rect.left / 3) + (2 * rect.right / 3)
    in
        case (compare height prev.height, compare next.height height) of
            (GT, GT) -> [Pos mid height]
            (LT, LT) -> [Pos mid height]
            (LT, GT) -> [Pos midLeft height, Pos midRight height]
            (GT, LT) -> [Pos midLeft height, Pos midRight height]
            (EQ, EQ) -> [Pos rect.left height]
            (GT, EQ) -> [Pos midLeft height]
            (EQ, LT) -> [Pos rect.left height, Pos midRight height]
            (EQ, GT) -> [Pos rect.left height, Pos midRight height]
            (LT, EQ) -> [Pos midLeft height]

-- Take a list of rectangles and bracket it (one zero-height rect on the
-- start and end).

type End = Front | Back

bracketRects : List Rect -> List Rect
bracketRects rects =
    Util.bracketMap
        (bracketRects1 Front)
        (bracketRects1 Back)
        rects

bracketRects1 : End -> Rect -> Rect
bracketRects1 end rect =
    let
        width = rect.right - rect.left
    in
        case end of
            Front ->
                Rect (rect.left - width) rect.left 0
            Back ->
                Rect rect.right (rect.right + width) 0
