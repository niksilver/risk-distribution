module Axis exposing (Scale, scale, viewXAxis)

import Util exposing (Transformer)

import Svg exposing (Svg)
import Svg.Attributes as SvgA


-- Module for working out a nice scale for an x- or y-axis

type alias Scale =
    { min : Float, max : Float, step : Float }


-- Some constants


strokeColour : String
strokeColour = "black"

strokeWidth : String
strokeWidth = "2"

tickMarkLength : Float
tickMarkLength = 10

-- Calculating a scale

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


-- View


viewXAxis : Transformer -> Scale -> Svg x
viewXAxis transformer scale =
    Svg.g []
    [ viewXAxisLine transformer scale
    , viewXAxisTicks transformer scale
    ]

viewXAxisLine : Transformer -> Scale -> Svg x
viewXAxisLine transformer scale =
    Svg.line
    [ SvgA.x1 (toString (transformer.trX scale.min))
    , SvgA.y1 (toString (transformer.trY 0))
    , SvgA.x2 (toString (transformer.trX scale.max))
    , SvgA.y2 (toString (transformer.trY 0))
    , SvgA.stroke strokeColour
    , SvgA.strokeWidth strokeWidth
    , SvgA.strokeLinecap "square"
    ]
    []

viewXAxisTicks : Transformer -> Scale -> Svg x
viewXAxisTicks transformer scale =
    Svg.g []
    (viewXAxisTicks' transformer scale scale.min [])

viewXAxisTicks' : Transformer -> Scale -> Float -> List (Svg x) -> List (Svg x)
viewXAxisTicks' transformer scale x accum =
    if (x > scale.max) then
        accum
    else
        viewXAxisTicks'
            transformer
            scale
            (x + scale.step) 
            (viewXAxisOneTick transformer scale x :: accum)

viewXAxisOneTick : Transformer -> Scale -> Float -> Svg x
viewXAxisOneTick transformer scale x =
    let
        x' = transformer.trX x
        y' = transformer.trY 0
    in
        Svg.line
        [ SvgA.x1 (toString x')
        , SvgA.y1 (toString y')
        , SvgA.x2 (toString x')
        , SvgA.y2 (toString (y' + tickMarkLength))
        , SvgA.stroke strokeColour
        , SvgA.strokeWidth strokeWidth
        ]
        []

