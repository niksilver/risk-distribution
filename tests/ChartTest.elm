module ChartTest exposing (all)

import Chart exposing (..)
import Distribution exposing (Limit(AtMost, AtLeast))

import ElmTest exposing (..)

all : Test
all =
    suite "ChartTest"
    [ rawSpecTest
    , transformXTest
    , transformYTest
    ]

{- Some layers...

   Probs:       0.15   0.25   0.10   0.40   0.10
             |-------|------|------|------|------|
   Ranges:  20.0    50.0   85.0   110.0  200.0  300.0

   Layers (with probs):
     y1      (--- 1.0
     y2              (--- 0.85
     y3                            (--- 0.50
     y4                                   (--- 0.10
     y5      0.15 ---)
     y6             0.40 ---)
     y7                    0.50 ---)
     y8                                   1.0 ---)

  We will list those out of order...
-}

y1 = { prob = 1.00, limit = AtLeast, value = 20.0 }
y2 = { prob = 0.85, limit = AtLeast, value = 50.0 }
y3 = { prob = 0.50, limit = AtLeast, value = 110.0 }
y4 = { prob = 0.10, limit = AtLeast, value = 200.0 }
y5 = { prob = 0.15, limit = AtMost, value = 50.0 }
y6 = { prob = 0.40, limit = AtMost, value = 85.0 }
y7 = { prob = 0.50, limit = AtMost, value = 110.0 }
y8 = { prob = 1.00, limit = AtMost, value = 300.0 }


-- Tests

rawSpecTest : Test
rawSpecTest =
    suite "rawspecTest"

    [ test "Chart with one rectangle should have area of its probability (1)" <|
      assertEqual
      (Just 0.15)  -- The probability
      ( [ y1, y5 ]
          |> rawSpec
          |> Maybe.map .rects
          |> Maybe.withDefault []
          |> List.head
          |> Maybe.map .height
          |> Maybe.map (\h -> h * (50.0 - 20.0))  -- Rectangle's area
      )

    , test "Chart with one rectangle should have area of its probability (2)" <|
      assertEqual
      (Just 0.35)  -- The probability
      ( [ y2, y7 ]
          |> rawSpec
          |> Maybe.map .rects
          |> Maybe.withDefault []
          |> List.head
          |> Maybe.map .height
          |> Maybe.map (\h -> h * (110.0 - 50.0))  -- Rectangle's area
      )

    , test "Chart should list rectangles in order" <|
      -- We're ignoring the height as lost accuracy confuses equality
      assertEqual
      [ { left = 20.0, right = 50.0 }
      , { left = 50.0, right = 85.0 }
      , { left = 85.0, right = 110.0 }
      , { left = 110.0, right = 200.0 }
      , { left = 200.0, right = 300.0 }
      ]
      ( [ y3, y2, y6, y8, y1, y4, y5, y7 ]
          |> rawSpec
          |> Maybe.map .rects
          |> Maybe.withDefault []
          |> List.map (\layer -> { left = layer.left, right = layer.right })
      )

    ]

transformXTest : Test
transformXTest =
    let
        viewDims = { left = 3, top = 8, width = 1000, height = 50 }
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
        viewDims = { left = 3, top = 8, width = 1000, height = 50 }
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

