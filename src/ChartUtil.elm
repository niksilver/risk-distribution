module ChartUtil exposing
    ( Rect, Spec, ViewDims, Transformer
    , transformX, transformY, scaleX, scaleY, transformer
    , mergeSimilar
    , curvePointsForRect
    , bracketRects
    )

import Spline exposing (Pos)


-- Specification for a chart

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

-- Merge rectangles in a spec that are similar in height
-- (i.e. within a certain proportion of the max height).
-- This might be useful when we're trying to draw a curve
-- as the distribution, and we don't want to confuse two
-- rectangles next to each other that are essentially two
-- parts of the same rectangle.

mergeSimilar : Float -> Spec -> Spec
mergeSimilar tolerance spec =
    mergeSimilar' (tolerance * spec.maxY) spec []

mergeSimilar' : Float -> Spec -> List Rect -> Spec
mergeSimilar' delta spec accum =
    case spec.rects of
        [] ->
            { spec | rects = List.reverse accum }
        head :: tail ->
            let
                spec' = { spec | rects = tail }
                accum' = mergeOrPutRect delta head accum
            in
                mergeSimilar' delta spec' accum'

mergeOrPutRect : Float -> Rect -> List Rect -> List Rect
mergeOrPutRect delta nextRect rects =
    case rects of
        [] -> [ nextRect ]
        prevRect :: tail ->
            let
                (lowerRect, upperRect) =
                    if (prevRect.height < nextRect.height) then
                        (prevRect, nextRect)
                    else
                        (nextRect, prevRect)
                lowerHeight = lowerRect.height
                upperHeight = upperRect.height
                diff = upperHeight - lowerHeight
                left = prevRect.left
                right = nextRect.right
                width = right - left
                upperWidth = upperRect.right - upperRect.left
            in
                if (diff <= delta) then
                    { left = left
                    , right = right
                    , height = lowerHeight + (diff * upperWidth/width)
                    }
                    :: tail
                else
                    nextRect :: rects

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
    frontBracketRects Front proportion rects
        |> List.reverse
        |> frontBracketRects Back proportion
        |> List.reverse

frontBracketRects : End -> Float -> List Rect -> List Rect
frontBracketRects end proportion rects =
    case rects of
        [] ->
            []
        head :: tail ->
            let
                height = head.height * proportion
                width = head.right - head.left
                head' =
                    case end of
                        Front -> Rect (head.left - width) head.left height
                        Back -> Rect head.right (head.right + width) height
            in
                head' :: rects

