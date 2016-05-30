module Distribution exposing
    ( Layer(Layer), Limit(AtLeast, AtMost), Interval(Closed, Open)
    , interval
    , sortIntervals
    , bestGreaterCounterpart, bestLesserCounterpart
    , intervals
    )

-- A layer of a distribution

type Layer =
    Layer
        { prob : Float
        , limit : Limit
        , value : Float
        }

type Limit = AtLeast | AtMost

-- How to layers overlap (if at all)

type Interval
    = Closed { lower : Float, upper : Float, prob : Float }
    | Open

-- Deduce an interval, if any, given two layers

interval : Layer -> Layer -> Interval
interval (Layer layer1) (Layer layer2) =
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
        Closed
            { lower = earlier.value
            , upper = later.value
            , prob = probability |> to4Dp
            }

-- Round a float to 4 decimal places, to avoid silly numbers due
-- to precision loss.

to4Dp : Float -> Float
to4Dp x =
    (x * 1000 |> round |> toFloat) / 1000

-- Sort a list of intervals.
-- Open intervals come at the end.
-- Closed intervals are ordered first by the lower bound, then the upper bound.
-- The probability is ignored.

sortIntervals : List Interval -> List Interval
sortIntervals x1 =
    let
        -- Compare two intervals; open intervals are "larger"
        comp : Interval -> Interval -> Order
        comp int1 int2 =
            case (int1, int2) of
                (Open, Open) -> EQ
                (Open, Closed _) -> GT
                (Closed _, Open) -> LT
                (Closed c1, Closed c2) ->
                    let
                        lowerComp = compare c1.lower c2.lower
                    in
                        if (lowerComp == EQ) then
                            compare c1.upper c2.upper
                        else
                            lowerComp
    in
        List.sortWith comp x1

-- Find a layer's best counterpart for determining an interval,
-- that's greater than this one

bestGreaterCounterpart : Layer -> List Layer -> Maybe Layer
bestGreaterCounterpart y ys =
    ys
        |> greaterThanAndOpposite y
        |> sortByValue
        |> List.head

greaterThanAndOpposite : Layer -> List Layer -> List Layer
greaterThanAndOpposite y ys =
    let
        greaterThan (Layer d1) (Layer d2) =
            d1.value > d2.value
        opposite (Layer d1) (Layer d2) =
            d1.limit /= d2.limit
    in
        List.filter (\y2 -> greaterThan y2 y && opposite y2 y) ys

sortByValue : List Layer -> List Layer
sortByValue ys =
    let
        value (Layer d) = d.value
    in
        List.sortBy value ys

-- Find a layer's best counterpart for determining an interval,
-- that's less than this one

bestLesserCounterpart : Layer -> List Layer -> Maybe Layer
bestLesserCounterpart y ys =
    ys
        |> lessThanAndOpposite y
        |> sortByDecreasingValue
        |> List.head

lessThanAndOpposite : Layer -> List Layer -> List Layer
lessThanAndOpposite y ys =
    let
        lessThan (Layer d1) (Layer d2) =
            d1.value < d2.value
        opposite (Layer d1) (Layer d2) =
            d1.limit /= d2.limit
    in
        List.filter (\y2 -> lessThan y2 y && opposite y2 y) ys

sortByDecreasingValue : List Layer -> List Layer
sortByDecreasingValue ys =
    let
        decreasingValue (Layer d) = -1 * d.value
    in
        List.sortBy decreasingValue ys

-- Get all the (closed) intervals given some layers

intervals : List Layer -> List Interval
intervals ys =
    let
        counterpart (Layer desc) =
            List.filter (\(Layer desc2) -> desc2.value > desc.value) ys
                |> List.sortBy (\(Layer desc2) -> desc2.value)
                |> List.head
        toInterval layer =
            case (counterpart layer) of
                Nothing -> []
                Just layer2 -> [interval layer layer2]
    in
        List.map toInterval ys
            |> List.concat

-- Get all the pairs of elements of a list

pairs : List a -> List (a, a)
pairs xs =
    pairs' xs []

pairs' : List a -> List (a, a) -> List (a, a)
pairs' xs accum =
    case xs of
        [] -> accum
        x1 :: tail ->
            let
                headPairs = List.map (\x2 -> (x1, x2)) tail
            in
                pairs' tail (List.append headPairs accum)

