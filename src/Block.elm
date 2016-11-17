module Block exposing
    ( Block, Rect, ChartBars, OverlayBlock
    , trim, taperBlocks, taperZoneWidth
    , taperComparator, toChartBars
    , toOverlayBlock
    )

-- Module to help work out the shapes needed to lay out and display
-- a probability curve, based on zones of various percentage values.

import Zone exposing (inf, Zone)
import Value exposing (Value (Exactly, Maximum, Contradiction))
import Util


-- A block representing a zone in a probability distribution

type alias Block =
    { zone : Zone
    , value : Value
    }

-- A rectangle on a chart

type alias Rect =
    { left : Float
    , right : Float
    , height : Float
    }

-- Bars illustrating a zone in a probability distribution, to be laid
-- out on a chart

type alias ChartBars =
    List Rect

-- A block representing an area on a chart where we can overlay information.
-- It corresponds to a Block, but it will be within the bounds of a chart.

type alias OverlayBlock =
    { zone : Zone
    , value : Value
    , rect : Rect
    }

-- Trim a list of zones, removing zero-height ones from the start and end

trim : List Block -> List Block
trim blocks =
    trimFront blocks
        |> List.reverse
        |> trimFront
        |> List.reverse

trimFront : List Block -> List Block
trimFront blocks =
    case blocks of
        [] ->
            []
        block :: tail ->
            case block.value of
                Exactly 0 src ->
                    trimFront tail
                Maximum 0 src ->
                    trimFront tail
                _ ->
                    blocks

-- When we want to taper a zone off to infinity we'll do it as a series
-- of rectangles. These will be:
-- 1/2 height + 1/4 height + 1/8 height + ...
-- The number of such rectangles we use in practice is this value

taperBlocks : Int
taperBlocks = 8

-- Say we have two zones; the first is infinitely long, the second is
-- finite, and both have a %age probability. We want to represent the
-- first as a series of reducing rectangles. This function tells us the
-- width of each such rectangle.
--
-- E.g. Zone -inf to 0 of probability 10 (pc1)
-- and zone of length 5 of probability 15 (pc2).
-- We want a charge that will look like this:
--
--               ----
--               |  |
--              .|  |  Area under bar = 15
--              .|  |  Area under curve = 10 (approx)
--             . |  |  Curve starts half way up the bar
--        . .    |  |  and is a series of rects, each of half the height
--        -----------  of the previous one.
--                     So we rely on the fact that a rectangle equals
-- 1/2 itself + 1/4 itself + 1/8 itself + ...
-- And those are the rectangles we stack next to each other to get the
-- taper. We don't do this forever. We only do it taperBlocks times, which
-- we say is close enough. And our first (tallest) rectangle will be
-- half the height of the neighbouring one, which means its width needs
-- to be the same width but then scaled to the relative percentages (pc1/pc2).
--
-- If the height of the neighbour is zero then we assume some default value.

taperZoneWidth : Int -> Float -> Int -> Float
taperZoneWidth pc1 width2 pc2 =
    let
        pc1' = toFloat pc1
        pc2' =
            if (pc2 == 0) then 10.0 else toFloat pc2
    in
        width2 * pc1' / pc2'

-- Given a tapering block among an ordered list of blocks
-- (which will be either the first or the last of them) pick the one
-- (or make one up) which we imagine our tapering chart blocks will sit
-- beside by means of comparison. We will use this to size the tapering
-- chart blocks appropriately. This assumes we are given a tapering
-- (i.e. infinite) block and the list of blocks is ordered correctly,
-- otherwise the output of this is meaningless

taperComparator : Block -> List Block -> Block
taperComparator block blocks =
    let
        default = { zone = Zone 0 1, value = Exactly 1 [] }
        orderedBlocks =
            if (block.zone.from == -inf) then
                blocks
            else
                List.reverse blocks
        hasPercent block =
            Value.percent block.value /= Nothing
        isNonZero block =
            Value.percent block.value /= Just 0
        isFinite block =
            Zone.isFinite block.zone
        isComparator b =
            isNonZero b && hasPercent b && isFinite b
        comparators =
            List.filter isComparator orderedBlocks
    in
        List.head comparators
            |> Maybe.withDefault default

-- Convert a block to one (or more) bars that can be laid out on a chart.
-- Mostly this will be one rect, but a zone tapering off to infinity
-- will be represented by several chart bars.

toChartBars : Block -> List Block -> ChartBars
toChartBars block blocks =
    if (Value.percent block.value == Nothing) then
        [ zeroRect block ]
    else if (Zone.isFinite block.zone) then
        [ finiteRect block ]
    else if (block.zone.from == -inf) then
        taperRange
            |> List.map (makeTaperingBar block blocks -1)
            |> List.reverse
    else
        taperRange
            |> List.map (makeTaperingBar block blocks 1)

-- Create a zero-height rect for a Block.
-- Used when the Value of a Block is a Contradiction.

zeroRect : Block -> Rect
zeroRect block =
    { left = block.zone.from
    , right = block.zone.to
    , height = 0
    }

-- Get the percent of a value, using a default if the value is
-- a contradiction

percentWithDefault : Value -> Int
percentWithDefault value =
    Value.percent value |> Maybe.withDefault -1

-- From a finite Block create an appropriate Rect

finiteRect : Block -> Rect
finiteRect block =
    let
        from = block.zone.from
        to = block.zone.to
        pc = percentWithDefault block.value
    in
        { left = from
        , right = to
        , height = (toFloat pc) / (to - from)
        }

-- Make list [0, 1, 2, ...] which has length `taperBlocks`

taperRange : List Int
taperRange =
    let
        base = List.repeat taperBlocks 0
    in
        List.indexedMap (\i elt -> i) base

-- Given an infinite Block tapering to the left (dir = -1) or right (dir = 1),
-- make a bar that is one of those that makes the tapering.
-- The index is which Rect this is:
-- idx = 0 means the tallest,
-- idx = 1 means the second tallest, etc.

makeTaperingBar : Block -> List Block -> Int -> Int -> Rect
makeTaperingBar block blocks dir idx =
    let
        pc = percentWithDefault block.value
        bFrom = block.zone.from
        bTo = block.zone.to

        neighbour = taperComparator block blocks
        nFrom = neighbour.zone.from
        nTo = neighbour.zone.to
        nWidth = nTo - nFrom
        nPc = percentWithDefault neighbour.value

        width = taperZoneWidth pc nWidth nPc
        shrinkage = 2 ^ (idx + 1)
        nearDistance = (idx * width)
        farDistance = (idx * width) + width
        (left, right) =
            if (dir == -1) then
                (bTo - farDistance, bTo - nearDistance)
            else
                (bFrom + nearDistance, bFrom + farDistance)
        height = (toFloat pc / width) / shrinkage
    in
        Rect left right height

-- Covert a Block to an OverlayBlock, give the bounds we should be working in

toOverlayBlock : Float -> Float -> Float -> Block -> OverlayBlock
toOverlayBlock minX maxX maxY block =
    { zone = block.zone
    , value = block.value
    , rect =
        { left = max minX block.zone.from
        , right = min maxX block.zone.to
        , height = maxY
        }
    }
