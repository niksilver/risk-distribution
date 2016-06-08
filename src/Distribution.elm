module Distribution exposing
    ( Layer, Limit(AtLeast, AtMost), Interval
    , interval
    , intervals
    , range
    )

-- A layer of a distribution

type alias Layer =
    { prob : Float
    , limit : Limit
    , value : Float
    }

type Limit = AtLeast | AtMost

-- How two layers overlap to produce a region of probability

type alias Interval
    = { lower : Float, upper : Float, prob : Float }

-- Deduce an interval, if any, given two layers

interval : Layer -> Layer -> Interval
interval layer1 layer2 =
    let
        (earlier, later) =
            if (layer1.value < layer2.value) then
                (layer1, layer2)
            else
                (layer2, layer1)
        probability =
            case (layer1.limit, layer2.limit) of
                (AtLeast, AtLeast) -> earlier.prob - later.prob
                (AtMost, AtMost) -> later.prob - earlier.prob
                (AtLeast, AtMost) -> layer1.prob + layer2.prob - 1
                (AtMost, AtLeast) -> layer1.prob + layer2.prob - 1
    in
        { lower = earlier.value
        , upper = later.value
        , prob = probability |> to4Dp
        }

-- Round a float to 4 decimal places, to avoid silly numbers due
-- to precision loss.

to4Dp : Float -> Float
to4Dp x =
    (x * 1000 |> round |> toFloat) / 1000

-- Get all the set of shortest (closed) intervals given some layers

intervals : List Layer -> List Interval
intervals ys =
    let
        laterLayer layer layer2 = layer2.value > layer.value
        layerValue layer = layer.value
        bestInterval y =
            List.filter (laterLayer y) ys
                |> List.sortBy layerValue
                |> List.head
        toInterval layer =
            case (bestInterval layer) of
                Nothing -> []
                Just layer2 -> [interval layer layer2]
    in
        List.map toInterval ys
            |> List.concat

-- Get the min and max of a number of intervals

range : List Interval -> Maybe (Float, Float)
range ints =
    let
        extract : (Maybe Float, Maybe Float) -> Maybe (Float, Float)
        extract (a, b) =
            case (a, b) of
                (Nothing, _) -> Nothing
                (_, Nothing) -> Nothing
                (Just x, Just y) -> Just (x, y)
        maybeMin = List.map .lower ints |> List.minimum
        maybeMax = List.map .upper ints |> List.maximum
    in
        extract (maybeMin, maybeMax)
