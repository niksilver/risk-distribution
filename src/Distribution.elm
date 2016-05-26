module Distribution exposing
    ( Layer(Layer), Limit(AtLeast, AtMost), Interval(Closed, Open)
    , interval
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
    = Closed Float Float
    | Open

-- Deduce an interval, if any, given two layers

interval : Layer -> Layer -> Interval
interval (Layer layer1) (Layer layer2) =
    if (layer1.limit == layer2.limit) then
        Open
    else
        Closed (min layer1.value layer2.value) (max layer1.value layer2.value)

