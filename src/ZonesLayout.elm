module ZonesLayout exposing
    ( Block, ChartBlock
    , trim, taperFactor, taperZoneWidth
    , toChartBlock
    )

-- Module to help work out the shapes needed to lay out and display
-- a probability curve, based on zones of various percentage values.

import Zone exposing (inf, Zone)
import ZoneDict exposing (Value (Exactly, Maximum, Contradiction))
import ChartUtil exposing (Rect)
import Util


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
    if (Zone.isFinite block.zone) then
        [ addRectToBlock block ]
    else
        [ makeTaperingChartBlock block blocks ]

-- Get the percent size of a Value

percent : Value -> Int
percent value =
    case value of
        Exactly p src -> p
        Maximum p src -> p
        Contradiction src -> -1

-- Turn a finite Block into a ChartBlock by adding an appropriate Rect

addRectToBlock : Block -> ChartBlock
addRectToBlock block =
    let
        from = block.zone.from
        to = block.zone.to
        pc = percent block.value
    in
        { zone = block.zone
        , value = block.value
        , rect =
            { left = from
            , right = to
            , height = (toFloat pc) / (to - from)
            }
        }

-- Given an infinite Block, make a ChartBlock that is one of those
-- that makes the tapering.

makeTaperingChartBlock : Block -> List Block -> ChartBlock
makeTaperingChartBlock block blocks =
    let
        pc = percent block.value
        neighbour =
            case (findNextBlock block blocks) of
                Just b -> b
                Nothing -> block
        nFrom = neighbour.zone.from
        nTo = neighbour.zone.to
        nWidth = nTo - nFrom
        nPc = percent neighbour.value
        width = taperZoneWidth pc nWidth nPc
    in
        { zone = block.zone
        , value = block.value
        , rect =
            { left = nFrom - width
            , right = nFrom
            , height = (toFloat pc / width) / 2
            }
        }

-- Find the block after the given one

findNextBlock : Block -> List Block -> Maybe Block
findNextBlock block blocks =
    case blocks of
        head :: tail ->
            if (head == block) then
                List.head tail
            else
                findNextBlock block tail
        [] ->
            Nothing
