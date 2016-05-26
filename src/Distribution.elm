module Distribution exposing
    ( Layer(Layer), Limit(AtLeast, AtMost), Interval(Closed, Open)
    , interval, bestGreaterCounterpart
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
    if (layer1.limit == layer2.limit) then
        Open
    else
        Closed
            { lower = min layer1.value layer2.value
            , upper = max layer1.value layer2.value
            , prob = layer1.prob + layer2.prob - 1 |> to4Dp
            }

-- Round a float to 4 decimal places, to avoid silly numbers due
-- to precision loss.

to4Dp : Float -> Float
to4Dp x =
    (x * 1000 |> round |> toFloat) / 1000

-- Given find a layer's best counterpart for determining an interval,
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

