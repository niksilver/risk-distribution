module ChartUtil exposing
    ( Rect, Spec, ViewDims, Transformer
    , transformX, transformY, scaleX, scaleY, transformer
    , mergeSimilar
    )


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
                lowerHeight = min nextRect.height prevRect.height
                upperHeight = max nextRect.height prevRect.height
                diff = upperHeight - lowerHeight
            in
                if (diff <= delta) then
                    { left = prevRect.left
                    , right = nextRect.right
                    , height = (lowerHeight + upperHeight) / 2
                    }
                    :: tail
                else
                    nextRect :: rects

