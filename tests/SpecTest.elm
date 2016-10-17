module SpecTest exposing (all)

import Spec exposing (..)

import Zone exposing (inf, Zone)
import Value exposing (Value(..))
import Block exposing (Rect)
import Spline exposing (Pos)

import ElmTest exposing (..)


all : Test
all =
    suite "SpecTest"
    [ scaleXTest
    , scaleYTest
    , transformXTest
    , transformYTest
    , curvePointsForRectTest
    , bracketRectsTest
    ]


scaleXTest : Test
scaleXTest =
    let
        viewDims =
            { left = 3
            , right = 3
            , top = 8
            , bottom = 8
            , width = 1000
            , height = 50
            }
        spec =
            { minX = 1
            , maxX = 5
            , maxY = 10
            , rects =
                [ { left = 1, right = 4, height = 8 }
                , { left = 4, right = 5, height = 10 }
                ]
            }
    in
    suite "scaleXTest"

    [ test "Scale x measure for zero length" <|
      assertEqual
      0
      (scaleX viewDims spec 0)

    , test "Scale x measure for full length" <|
      assertEqual
      1000
      (scaleX viewDims spec 4)

    , test "Scale x measure for middle length" <|
      assertEqual
      250
      (scaleX viewDims spec 1)

    ]

scaleYTest : Test
scaleYTest =
    let
        viewDims =
            { left = 3
            , right = 3
            , top = 8
            , bottom = 8
            , width = 1000
            , height = 50
            }
        spec =
            { minX = 1
            , maxX = 5
            , maxY = 10
            , rects =
                [ { left = 1, right = 4, height = 8 }
                , { left = 4, right = 5, height = 10 }
                ]
            }
    in
    suite "scaleYTest"

    [ test "Scale y measure for zero height" <|
      assertEqual
      0
      (scaleY viewDims spec 0)

    , test "Scale y measure for full height" <|
      assertEqual
      50
      (scaleY viewDims spec 10)

    , test "Scale y measure for middle height" <|
      assertEqual
      10
      (scaleY viewDims spec 2)

    ]


transformXTest : Test
transformXTest =
    let
        viewDims =
            { left = 3
            , right = 3
            , top = 8
            , bottom = 8
            , width = 1000
            , height = 50
            }
        spec =
            { minX = 1
            , maxX = 5
            , maxY = 10
            , rects =
                [ { left = 1, right = 4, height = 8 }
                , { left = 4, right = 5, height = 10 }
                ]
            }
    in
    suite "transformXTest"

    [ test "Transform x test for LHS" <|
      assertEqual
      3
      (transformX viewDims spec 1)

    , test "Transform x test for RHS" <|
      assertEqual
      1003
      (transformX viewDims spec 5)

    , test "Transform x test for middle" <|
      assertEqual
      (250 + 3)
      (transformX viewDims spec 2)

    ]

transformYTest : Test
transformYTest =
    let
        viewDims =
            { left = 3
            , right = 3
            , top = 8
            , bottom = 8
            , width = 1000
            , height = 50
            }
        spec =
            { minX = 1
            , maxX = 5
            , maxY = 10
            , rects =
                [ { left = 1, right = 4, height = 8 }
                , { left = 4, right = 5, height = 10 }
                ]
            }
    in
    suite "transformYTest"

    [ test "Transform y test for top" <|
      assertEqual
      8
      (transformY viewDims spec 10)

    , test "Transform y test for bottom" <|
      assertEqual
      58
      (transformY viewDims spec 0)

    , test "Transform y test for middle" <|
      assertEqual
      (8 + 40)
      (transformY viewDims spec 2)

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
      (bracketRects 1.0 [])

    , test "Bracketing three rects should give five rects total" <|
      assertEqual
      (5)
      ( bracketRects 0.5 [Rect 0 1 6, Rect 1 4 7, Rect 4 5 8]
          |> List.length
      )

    , test "Bracketing three rects should give a proportional one at the front" <|
      assertEqual
      (Just 3.0)
      ( bracketRects 0.5 [Rect 0 1 6, Rect 1 4 7, Rect 4 5 8]
          |> List.map .height
          |> List.head
      )

    , test "Bracketing three rects should give a proportional one at the end" <|
      assertEqual
      (Just 4.0)
      ( bracketRects 0.5 [Rect 0 1 6, Rect 1 4 7, Rect 4 5 8]
          |> List.map .height
          |> List.reverse
          |> List.head
      )

    , test "Front-bracketing rect should have width equal to the original front rect" <|
      assertEqual
      (Just (Rect -1 0 3.0))
      ( bracketRects 0.5 [Rect 0 1 6, Rect 1 4 7, Rect 4 5 8]
          |> List.head
      )

    , test "End-bracketing rect should have width equal to the original end rect" <|
      assertEqual
      (Just (Rect 6 8 4.0))
      ( bracketRects 0.5 [Rect 0 1 6, Rect 1 4 7, Rect 4 6 8]
          |> List.reverse
          |> List.head
      )

    ]
