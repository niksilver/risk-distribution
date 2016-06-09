module ChartTest exposing (all)

import Chart exposing (..)
import Distribution exposing (Limit(AtMost, AtLeast))

import ElmTest exposing (..)

all : Test
all =
    suite "ChartTest"
    [ rawSpecTest
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

    ]

