module Axis exposing (Scale, scale)

-- Module for working out a nice scale for an x- or y-axis

type alias Scale =
    { min : Float, max : Float, step : Float }

scale : Float -> Float -> Int -> Scale
scale lower upper ticks =
    let
        -- From http://stackoverflow.com/questions/326679/choosing-an-attractive-linear-scale-for-a-graphs-y-axis

        range = upper - lower
        unroundedTickSize = range / (ticks - 1 |> toFloat)
        x = ceiling ((logBase 10 unroundedTickSize) - 1)
        pow10x = 10 ^ x |> toFloat
        roundedTickRange = toFloat (ceiling (unroundedTickSize / pow10x)) * pow10x

    in
        { min = roundedTickRange * toFloat (floor (lower / roundedTickRange))
        , max = roundedTickRange * toFloat (floor (lower / roundedTickRange)) + ((toFloat ticks - 1) * roundedTickRange)
        , step = roundedTickRange
        }

