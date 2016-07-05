module Spline exposing (Pos, spline)

-- Generating Cardinal splines.
-- See http://codeplea.com/introduction-to-splines

-- A simple (x,y) type

type alias Pos = { x : Float, y : Float}

-- Hermite basis functions

h1 : Float -> Float
h1 t =
    2 * t^3 - 3 * t^2 + 1

h2 : Float -> Float
h2 t =
    -2 * t^3 + 3 * t^2

h3 : Float -> Float
h3 t =
    t^3 - 2 * t^2 + t

h4 : Float -> Float
h4 t =
    t^3 - t^2

-- Calculate the value of y at a point in the interval
-- m1 and m2 are the tangent values.
-- y1 and y2 are the values at either end of the interval.
-- t goes from 0 to 1.

intervalValue : Float -> Float -> Float -> Float -> Float -> Float
intervalValue m1 m2 y1 y2 t =
    m1 * (h3 t) + y1 * (h1 t) + y2 * (h2 t) + m2 * (h4 t)

-- Calculate the points on a curve between two points a and b,
-- and given that we want the curve to consist of a given number of lines.

spline : Int -> Pos -> Pos -> List Pos
spline lines a b =
    spline' 0 lines a b [a]

spline' : Int -> Int -> Pos -> Pos -> List Pos -> List Pos
spline' idx lines a b accum =
    let
        t = toFloat idx / toFloat lines
        x = a.x + t * (b.x - a.x)
        y = intervalValue 1 1 a.y b.y t
        accum' = Pos x y :: accum
        idx' = idx + 1
    in
        if (idx' > lines) then
            List.reverse accum'
        else
            spline' idx' lines a b accum'

