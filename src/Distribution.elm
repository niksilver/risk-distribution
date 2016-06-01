module Distribution exposing
    ( Layer(Layer), Limit(AtLeast, AtMost), Interval
    , interval
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

-- How two layers overlap

type alias Interval
    = { lower : Float, upper : Float, prob : Float }

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
        laterLayer (Layer desc) (Layer desc2) = desc2.value > desc.value
        layerValue (Layer desc) = desc.value
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

