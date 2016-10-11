module ZonesLayout exposing
    ( Block, ChartBlock
    , trim, taperFactor, taperZoneWidth
    , toChartBlock
    , toRange
    )

-- Module to help work out the shapes needed to lay out and display
-- a probability curve, based on zones of various percentage values.

import Zone exposing (inf, Zone)
import ZoneDict exposing (Value (Exactly, Maximum, Contradiction))
import ChartUtil exposing (Rect)


-- A block representing a zone in a probability distribution

type alias Block =
    { zone : Zone
    , value : Value
    }

-- A block representing a zone in a probability distribution, to be laid
-- out on a chart

type alias ChartBlock =
    { zone : Zone
    , value : Value
    , rect : Rect
    }

-- Things to add:
--   - Convert a finite zone to a rect
--   - Calculate the height of the tallest rect of an infinite zone
--   - Convert an infinite zone to a series of rects

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

taperFactor : Float
taperFactor = 5

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
-- taper. We don't do this forever. We only do it taperFactor times, which
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

-- Convert a block to one (or more) that can be laid out on a chart.
-- Mostly this will be done block, but a zone tapering off to infinity
-- will be represented by several chart blocks.

toChartBlock : Block -> List Block -> List ChartBlock
toChartBlock block blocks =
    []

-- Convert a list of zone/value pairs into a range for a chart's x-axis

toRange : List (Zone, Value) -> (Float, Float)
toRange entries =
    entries
        |> List.map fst
        |> minMax
        |> fixMinusInf

minMax : List Zone -> (Float, Float)
minMax zones =
    ( zones
        |> List.head |> Maybe.map .from |> Maybe.withDefault -1
    , zones |> List.reverse
        |> List.head |> Maybe.map .to |> Maybe.withDefault 1
    )

fixMinusInf : (Float, Float) -> (Float, Float)
fixMinusInf (min, max) =
    if (min == -inf && max > 0) then
        (-max, max)
    else if (min == -inf && max < 0) then
        (2 * max, max)
    else if (min == -inf && max == 0) then
        (-1, max)
    else
        (min, max)
