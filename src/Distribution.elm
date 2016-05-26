module Distribution exposing
    ( Layer(Layer), Limit(AtLeast, AtMost), Overlap(Closed)
    , overlap
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

type Overlap
    = Closed Float Float

-- Find the overlap, if any, between two layers

overlap : Layer -> Layer -> Overlap
overlap layer1 layer2 =
    Closed 7.0 123.0

