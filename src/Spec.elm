module Spec exposing
    ( Spec, ViewDims, Transformer
    , fromSegments, rects
    , transformX, transformY, scaleX, scaleY, transformer
    , curvePointsForRect
    , bracketRects
    )

import Segment exposing (Segment)
import Derivation
import DerivationScheme
import Block exposing (Rect, ChartBlock)
import ZoneDict
import Spline exposing (Pos)
import Util


-- Specification for a chart

type alias Spec =
    { minX : Float
    , maxX : Float
    , maxY : Float
    , blocks : List ChartBlock
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


-- Create a spec from some segments.
-- It isn't scaled at all.
-- Will produce Nothing if the input is an empty list

fromSegments : List Segment -> Maybe Spec
fromSegments segments =
    let
        dScheme = DerivationScheme.scheme segments
        zones = dScheme.zones
        derivations = dScheme.derivations
        entries =
            ZoneDict.fill zones derivations
                |> ZoneDict.toList
        toBlock (zone, value) =
            { zone = zone, value = value }
        blocks =
            List.map toBlock entries
                |> Block.trim
        chartBlocks =
            List.map (\b -> Block.toChartBlock b blocks) blocks
                |> List.concat
        maybeMinX =
            chartBlocks |> List.map .rect |> List.map .left |> List.minimum
        maybeMaxX =
            chartBlocks |> List.map .rect |> List.map .right |> List.maximum
        maybeMaxY =
            chartBlocks |> List.map .rect |> List.map .height |> List.maximum
        toSpec minX maxX maxY =
            { minX = minX
            , maxX = maxX
            , maxY = maxY
            , blocks = chartBlocks
            }
    in
        Maybe.map3
            toSpec
            maybeMinX
            maybeMaxX
            maybeMaxY

-- Extract just the rects from a spec

rects : Spec -> List Rect
rects spec =
    List.map .rect spec.blocks

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
