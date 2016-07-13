module Spline exposing (Pos, spline, splines, yMinMax)

import Util

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

-- Calculate the tangents m1 and m2.
-- c is the tension parameter

tangents : Float -> Pos -> Pos -> Pos -> Pos -> (Float, Float)
tangents c p0 p1 p2 p3 =
    let
        dx = p2.x - p1.x
        dx1 = p1.x - p0.x
        dx2 = p3.x - p2.x
        s1 = 2 * (dx / (dx1 + dx))
        s2 = 2 * (dx / (dx + dx2))
        m1 = s1 * c * (p2.y - p0.y)
        m2 = s2 * c * (p3.y - p1.y)
    in
        (m1, m2)

-- Calculate the points on a curve between two points p1 and p2
-- with outer control points p0 and p3.
-- The curve should consist of a given number of lines.

spline : Int -> Pos -> Pos -> Pos -> Pos -> List Pos
spline lines p0 p1 p2 p3 =
    spline' 0 lines p0 p1 p2 p3 [p1]

spline' : Int -> Int -> Pos -> Pos -> Pos -> Pos -> List Pos -> List Pos
spline' idx lines p0 p1 p2 p3 accum =
    let
        tension = 0.5
        (m1, m2) = tangents tension p0 p1 p2 p3
        t = toFloat idx / toFloat lines
        x = p1.x + t * (p2.x - p1.x)
        y = intervalValue m1 m2 p1.y p2.y t
        accum' = Pos x y :: accum
        idx' = idx + 1
    in
        if (idx' > lines) then
            List.reverse accum'
        else
            spline' idx' lines p0 p1 p2 p3 accum'

-- Turn a series of points into a series of splines.
-- lines is the number of lines between each two points.

splines : Int -> List Pos -> List Pos
splines lines points =
    let
        points' = Util.bracket points
        quads =
            Util.sliding 4 points'
        toSpline : List Pos -> List Pos
        toSpline quad =
            case quad of
                [p0, p1, p2, p3] -> spline lines p0 p1 p2 p3
                _ -> []
    in
        quads
            |> List.map toSpline
            |> List.concat

-- Get the min and max y values

yMinMax : List Pos -> Maybe (Float, Float)
yMinMax ps =
    yMinMax' ps Nothing

yMinMax' : List Pos -> Maybe (Float, Float) -> Maybe (Float, Float)
yMinMax' ps accum =
    case ps of
        [] ->
            accum
        p :: tail ->
            let
                accum' =
                    case accum of
                        Nothing ->
                            Just (p.y, p.y)
                        Just (min', max') ->
                            Just (min min' p.y, max max' p.y)
            in
                yMinMax' tail accum'


