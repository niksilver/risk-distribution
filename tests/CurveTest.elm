module CurveTest exposing (all)

import Curve exposing (..)

import Block exposing (Rect)
import Spline exposing (Pos)

import ElmTest exposing (..)


all : Test
all =
    suite "CurveTest"
    [ curvePointsForRectTest
    , bracketRectsTest
    ]

curvePointsForRectTest : Test
curvePointsForRectTest =
    suite "curvePointsForRectTest"

    [ test "Rect in rise should give point in middle" <|
      assertEqual
      [Pos 1.5 4]
      (curvePointsForRect (Rect 0 1 0) (Rect 1 2 4) (Rect 2 10 5))

    , test "Rect in decline should give point in middle" <|
      assertEqual
      [Pos 4.5 3]
      (curvePointsForRect (Rect 1 4 10) (Rect 4 5 3) (Rect 5 7 1))

    , test "Rect in dip should give two points around middle" <|
      assertEqual
      [Pos 4.5 3, Pos 5.0 3]
      (curvePointsForRect (Rect 1 4 10) (Rect 4 5.5 3) (Rect 5.5 7 5))

    , test "Rect at peak should give two points around middle" <|
      assertEqual
      [Pos 2.5 3, Pos 3.0 3]
      (curvePointsForRect (Rect 1 2 1) (Rect 2 3.5 3) (Rect 3.5 7 2))

    , test "Rect on the flat should give one point on the left" <|
      assertEqual
      [Pos 2 5]
      (curvePointsForRect (Rect 1 2 5) (Rect 2 3 5) (Rect 3 4 5))

    , test "Rect rising to the flat should give one point left of middle" <|
      assertEqual
      [Pos 2.5 5]
      (curvePointsForRect (Rect 1 2 3) (Rect 2 3.5 5) (Rect 3.5 4 5))

    , test "Rect falling from the flat should give point on the left and point right of middle" <|
      assertEqual
      [Pos 2 5, Pos 3 5]
      (curvePointsForRect (Rect 1 2 5) (Rect 2 3.5 5) (Rect 3.5 4 3))

    , test "Rect rising from the flat should give point on the left and point right of middle" <|
      assertEqual
      [Pos 2 3, Pos 3 3]
      (curvePointsForRect (Rect 1 2 3) (Rect 2 3.5 3) (Rect 3.5 4 5))

    , test "Rect falling to the flat should give one point left of middle" <|
      assertEqual
      [Pos 3.5 3]
      (curvePointsForRect (Rect 1 3 5) (Rect 3 4.5 3) (Rect 4.5 5 3))

    ]

bracketRectsTest : Test
bracketRectsTest =
    suite "bracketRectsTest"

    [ test "Bracketing no rects should give no rects again" <|
      assertEqual
      []
      (bracketRects [])

    , test "Bracketing three rects should give five rects total" <|
      assertEqual
      (5)
      ( bracketRects [Rect 0 1 6, Rect 1 4 7, Rect 4 5 8]
          |> List.length
      )

    , test "Bracketing three rects should give a zero-height one at the front" <|
      assertEqual
      (Just 0)
      ( bracketRects [Rect 0 1 6, Rect 1 4 7, Rect 4 5 8]
          |> List.map .height
          |> List.head
      )

    , test "Bracketing three rects should give a zero-height one at the end" <|
      assertEqual
      (Just 0)
      ( bracketRects [Rect 0 1 6, Rect 1 4 7, Rect 4 5 8]
          |> List.map .height
          |> List.reverse
          |> List.head
      )

    , test "Front-bracketing rect should have width equal to the original front rect" <|
      assertEqual
      (Just (Rect -1 0 0))
      ( bracketRects [Rect 0 1 6, Rect 1 4 7, Rect 4 5 8]
          |> List.head
      )

    , test "End-bracketing rect should have width equal to the original end rect" <|
      assertEqual
      (Just (Rect 6 8 0))
      ( bracketRects [Rect 0 1 6, Rect 1 4 7, Rect 4 6 8]
          |> List.reverse
          |> List.head
      )

    ]
