module ZoneLayout exposing
    ( taperFactor, taperZoneWidth, toRange
    )

import Zone exposing (inf, Zone)
import ZoneDict exposing (PcValue)

-- When we want to taper a zone off to infinity we'll do it as a series
-- of rectangles. These will be:
-- 1/2 height + 1/4 height + 1/8 height + ...
-- The number of such rectangles we use in practice is this value

taperFactor : Float
taperFactor = 5

-- Module to help work out the shapes needed to lay out and display
-- a probability curve, based on zones of various percentage values.

-- Say we have two zones; the first is infinitely long, the second is
-- finite, and both have a %age probability. Then we need to work out
-- how big the truncated form of the first zone should be to represent
-- on a chart.
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
-- Question: What happens when the height of the neighbour (pc2) is zero?

taperZoneWidth : Int -> Float -> Int -> Float
taperZoneWidth pc1 width2 pc2 =
    let
        pc1' = toFloat pc1
        pc2' =
            if (pc2 == 0) then 10.0 else toFloat pc2
    in
        width2 * pc1' / pc2' * taperFactor

-- Convert a list of zone/value pairs into a range for a chart's x-axis

toRange : List (Zone, PcValue) -> (Float, Float)
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
