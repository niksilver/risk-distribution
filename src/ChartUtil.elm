module ChartUtil exposing
    ( Bias (Left, Both), Shape (Taper)
    , Rect, Spec, ViewDims, Transformer
    , AxisPoints (NoPoints, MinusInfToPoint, PointToInf, Range)
    , truncateRange, fromEntries
    , transformX, transformY, scaleX, scaleY, transformer
    , curvePointsForRect
    , bracketRects
    )

import Zone exposing (inf, Zone)
import ZoneDict exposing (Value (Exactly), PcValue)
import Spline exposing (Pos)
import Util


-- Shapes and specification for a chart

type Bias = Left | Both  -- Which way a taper is biased

type Shape =
    Taper
        { bias : Bias
        , from : Float
        , to : Float
        , area : Value Float
        }

type alias Rect =
    { left : Float
    , right : Float
    , height : Float
    }

type alias Spec =
    { minX : Float
    , maxX : Float
    , maxY : Float
    , rects : List Rect
    }

-- Dimensions for a viewBox

type alias ViewDims =
    { left : Float
    , right : Float
    , top : Float
    , bottom : Float
    , width : Float
    , height : Float
    }

-- A suite of functions that transform and scale co-ordinates

type alias Transformer =
    { trX : Float -> Float    -- Transform an x co-ordinate
    , trY : Float -> Float    -- Transform a y co-ordinate
    , scX : Float -> Float    -- Scale an x co-ordinate
    , scY : Float -> Float    -- Scale a y co-ordinate
    }


-- Find sensible limits for a range of values, for the x-axis.
-- Axis points represent the known limits on an axis. We may have zero
-- or more finite points, and it may taper off to infinity at either end.

type AxisPoints
    = NoPoints -- No points, or maybe just -inf to inf
    | MinusInfToPoint Float  -- Just -inf to a point
    | PointToInf Float  -- Just a point to inf
    | Range Bool Float Float Bool  -- A range, maybe -inf, maybe inf

truncateRange : AxisPoints -> (Float, Float)
truncateRange points =
    case points of
        NoPoints ->
            (-1, 1)
        MinusInfToPoint x ->
            if (x > 0) then
                (-x, x)
            else if (x < 0) then
                (2 * x, x)
            else
                (-1, 0)
        PointToInf x ->
            if (x < 0) then
                (x, -x)
            else if (x > 0) then
                (x, 2 * x)
            else
                (0, 1)
        Range mInf x1 x2 pInf ->
            case (mInf, pInf) of
                (False, False) -> (x1, x2)
                (True, False) -> (x1 - (x2 - x1)/5, x2)
                (False, True) -> (x1, x2 + (x2 - x1)/5)
                (True, True) -> (x1 - (x2 - x1)/5, x2 + (x2 - x1)/5)


-- Go from ZoneDict entries to a list of shapes for a chart

fromEntries : List (Zone, PcValue) -> List Shape
fromEntries entries =
    fromEntries' entries []

fromEntries' : List (Zone, PcValue) -> List Shape -> List Shape
fromEntries' entries accum =
    case entries of
        [] ->
            accum
        ({ from, to }, value) :: tail ->
            [ Taper
                { bias = calcBias from to
                , from = from
                , to = to
                , area = ZoneDict.toValueFloat value
                }
            ]

calcBias : Float -> Float -> Bias
calcBias from to =
    if (from == -inf && to == inf) then
        Both
    else
        Left

-- Scale a length on the x- or y-axis from a spec to a view box.

scaleX : ViewDims -> Spec -> Float -> Float
scaleX dims spec length =
    length / (spec.maxX - spec.minX) * dims.width

scaleY : ViewDims -> Spec -> Float -> Float
scaleY dims spec height =
    height / spec.maxY * dims.height


-- Transform a point on the x-axis or y-axis from its place in a spec
-- to a place in the chart view box.

transformX : ViewDims -> Spec -> Float -> Float
transformX dims spec x =
    dims.left + (x - spec.minX) / (spec.maxX - spec.minX) * dims.width

transformY : ViewDims -> Spec -> Float -> Float
transformY dims spec y =
    dims.top + (spec.maxY - y) / spec.maxY * dims.height

-- Create a suite of functions that transform and scale co-ordinates

transformer : ViewDims -> Spec -> Transformer
transformer viewDim spec =
    { trX = transformX viewDim spec
    , trY = transformY viewDim spec
    , scX = scaleX viewDim spec
    , scY = scaleY viewDim spec
    }

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

-- Take a list of rectangles and bracket it (one rect on the start and end).
-- Each new Rect should have a height which is the specified proportion
-- of its neighbour. E.g. A proportion of 0.5 means the new first Rect
-- will be half the height of the (original) first Rect and the new last
-- Rect will be half the height of the (original) last Rect.

type End = Front | Back

bracketRects : Float -> List Rect -> List Rect
bracketRects proportion rects =
    Util.bracketMap
        (bracketRects1 Front proportion)
        (bracketRects1 Back proportion)
        rects

bracketRects1 : End -> Float -> Rect -> Rect
bracketRects1 end proportion rect =
    let
        height = rect.height * proportion
        width = rect.right - rect.left
    in
        case end of
            Front ->
                Rect (rect.left - width) rect.left height
            Back ->
                Rect rect.right (rect.right + width) height
