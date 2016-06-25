module Axis exposing (Scale, scale)

-- Module for working out a nice scale for an x- or y-axis

type alias Scale =
    { min : Float, max : Float, step : Float }

scale : Float -> Float -> Int -> Scale
scale lower upper maxTicks =
    let
        -- From http://stackoverflow.com/questions/8506881/nice-label-algorithm-for-charts-with-minimum-ticks/16363437#16363437

        maxTicks' = toFloat maxTicks
        range = niceNum (upper - lower) False
        tickSpacing = niceNum (range / (maxTicks' - 1)) True
        niceMin = (floor >> toFloat) (lower / tickSpacing) * tickSpacing
        niceMax = (ceiling >> toFloat) (upper / tickSpacing) * tickSpacing

    in
        { min = niceMin
        , max = niceMax
        , step = tickSpacing
        }

niceNum : Float -> Bool -> Float
niceNum range round =
    let
        exponent = (floor >> toFloat) (logBase 10 range)
        fraction = range / (10 ^ exponent)
        niceFraction =
            case round of
                True ->
                    if (fraction < 1.5) then 1
                    else if (fraction < 3) then 2
                    else if (fraction < 7) then 5
                    else 10
                False ->
                    if (fraction <= 1) then 1
                    else if (fraction <= 2) then 2
                    else if (fraction <= 5) then 5
                    else 10
    in
        niceFraction * (10 ^ exponent)

