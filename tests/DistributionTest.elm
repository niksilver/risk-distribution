module DistributionTest exposing (all)

import Distribution as Dist exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "DistributionTest"
    [ intervalTestRange
    , intervalTestProbability
    , intervalsTest
    , rangeTest
    ]

 
-- Utilities -----------------------------------------------


assertSameIntervals : List Interval -> List Interval -> Assertion
assertSameIntervals x1 x2 =
    let
        x1Sorted = Dist.sort x1
        x2Sorted = Dist.sort x2
    in
        assertEqual x1Sorted x2Sorted

range' : Interval -> (Float, Float)
range' interval =
    (interval.lower, interval.upper)

-- Tests ----------------------------------------------------


intervalTestRange : Test
intervalTestRange =
    suite "intervalTest - open/closed range'"

    [ test "From (-- and --) overlapping we should recognise a closed interval (1)" <|
      let
          layer1 = { prob = 1.0, limit = AtLeast, value = 7.0 }
          layer2 = { prob = 1.0, limit = AtMost, value = 123.0 }
      in
          assertEqual
          (7.0, 123.0)
          (interval layer1 layer2 |> range')

    , test "From (-- and --) overlapping we should recognise a closed interval (2)" <|
      let
          layer1 = { prob = 1.0, limit = AtLeast, value = 3.4 }
          layer2 = { prob = 1.0, limit = AtMost, value = 6.7 }
      in
          assertEqual
          (3.4, 6.7)
          (interval layer1 layer2 |> range')

    , test "From --) and (-- overlapping we should recognise a closed interval" <|
      let
          layer1 = { prob = 1.0, limit = AtMost, value = 6.7 }
          layer2 = { prob = 1.0, limit = AtLeast, value = 3.4 }
      in
          assertEqual
          (3.4, 6.7)
          (interval layer1 layer2 |> range')

    , test "From --) and (-- not overlapping we should recognise a closed interval" <|
      let
          layer1 = { prob = 1.0, limit = AtMost, value = 55.0 }
          layer2 = { prob = 1.0, limit = AtLeast, value = 66.0 }
      in
          assertEqual
          (55.0, 66.0)
          (interval layer1 layer2 |> range')

    , test "From early (-- and later (-- we should recognise a closed interval" <|
      let
          layer1 = { prob = 1.0, limit = AtLeast, value = 3.4 }
          layer2 = { prob = 0.8, limit = AtLeast, value = 6.7 }
      in
          assertEqual
          (3.4, 6.7)
          (interval layer1 layer2 |> range')

    , test "From late (-- and earlier (-- we should recognise a closed interval" <|
      let
          layer1 = { prob = 0.75, limit = AtLeast, value = 6.7 }
          layer2 = { prob = 1.0, limit = AtLeast, value = 3.4 }
      in
          assertEqual
          (3.4, 6.7)
          (interval layer1 layer2 |> range')

    , test "From early --) and later --) we should recognise a closed interval" <|
      let
          layer1 = { prob = 0.6, limit = AtMost, value = 3.3 }
          layer2 = { prob = 1.0, limit = AtMost, value = 7.7 }
      in
          assertEqual
          (3.3, 7.7)
          (interval layer1 layer2 |> range')

    , test "From late --) and earlier --) we should recognise a closed interval" <|
      let
          layer1 = { prob = 1.0, limit = AtMost, value = 6.6 }
          layer2 = { prob = 0.3, limit = AtMost, value = 3.2 }
      in
          assertEqual
          (3.2, 6.6)
          (interval layer1 layer2 |> range')

    ]

intervalTestProbability : Test
intervalTestProbability =
    suite "intervalTest - probability"

    [ test "From (-- and --) overlapping we should get the right probability" <|
      let
          layer1 = { prob = 0.50, limit = AtLeast, value = 7.0 }
          layer2 = { prob = 0.70, limit = AtMost, value = 123.0 }
      in
          assertEqual
          (0.20)
          (interval layer1 layer2 |> .prob)

    , test "From --) and (-- overlapping we should get the right probability" <|
      let
          layer1 = { prob = 0.90, limit = AtMost, value = 123.0 }
          layer2 = { prob = 0.80, limit = AtLeast, value = 7.0 }
      in
          assertEqual
          (0.70)
          (interval layer1 layer2 |> .prob)

    , test "From --) and (-- apart we should get the right probability" <|
      let
          layer1 = { prob = 0.20, limit = AtMost, value = 7.0 }
          layer2 = { prob = 0.10, limit = AtLeast, value = 10.0 }
      in
          assertEqual
          (0.70)
          (interval layer1 layer2 |> .prob)

    , test "From --) and (-- apart reversed we should get the right prob" <|
      let
          layer1 = { prob = 0.10, limit = AtLeast, value = 10.0 }
          layer2 = { prob = 0.20, limit = AtMost, value = 7.0 }
      in
          assertEqual
          (0.70)
          (interval layer1 layer2 |> .prob)

    , test "From early (-- and later (-- we should get the right probability" <|
      let
          layer1 = { prob = 0.9, limit = AtLeast, value = 3.4 }
          layer2 = { prob = 0.8, limit = AtLeast, value = 6.7 }
      in
          assertEqual
          (0.1)
          (interval layer1 layer2 |> .prob)

    , test "From late (-- and earlier (-- we should get the right probability" <|
      let
          layer1 = { prob = 0.7, limit = AtLeast, value = 7.7 }
          layer2 = { prob = 0.9, limit = AtLeast, value = 3.3 }
      in
          assertEqual
          (0.2)
          (interval layer1 layer2 |> .prob)

    , test "From early --) and later --) we should get the right probability" <|
      let
          layer1 = { prob = 0.6, limit = AtMost, value = 3.3 }
          layer2 = { prob = 1.0, limit = AtMost, value = 7.7 }
      in
          assertEqual
          (0.4)
          (interval layer1 layer2 |> .prob)

    , test "From late --) and earlier --) we should get the right probability" <|
      let
          layer1 = { prob = 0.75, limit = AtMost, value = 8.8 }
          layer2 = { prob = 0.45, limit = AtMost, value = 4.6 }
      in
          assertEqual
          (0.30)
          (interval layer1 layer2 |> .prob)

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
layers = [ y7, y5, y3, y1, y2, y4, y6, y8 ]

intervalsTest : Test
intervalsTest =
    suite "intervalsTest"
    [ test "Given no layers, should return empty list" <|
      assertEqual
      []
      (intervals [])

    , test "Given one layer, should return empty list" <|
      assertEqual
      []
      (intervals [y1])

    , test "Given two intersecting layers, should work out single interval" <|
      assertEqual
      [ { lower = 20.0, upper = 50.0, prob = 0.15 } ]
      (intervals [y1, y5])

    , test "Given two intersecting layers reversed, should work out single interval" <|
      assertEqual
      [ { lower = 20.0, upper = 50.0, prob = 0.15 } ]
      (intervals [y5, y1])

    , test "Given two layers in same direction, should work out single interval" <|
      assertEqual
      [ { lower = 20.0, upper = 110.0, prob = 0.50 } ]
      (intervals [y1, y3])

    , test "Given three layers in same direction, should work out two intervals" <|
      assertSameIntervals
      [ { lower = 20.0, upper = 50.0, prob = 0.15 }
      , { lower = 50.0, upper = 110.0, prob = 0.35 }
      ]
      (intervals [y1, y2, y3])

    , test "Given four layers, should work out three intervals" <|
      assertSameIntervals
      [ { lower = 20.0, upper = 50.0, prob = 0.15 }
      , { lower = 50.0, upper = 110.0, prob = 0.35 }
      , { lower = 110.0, upper = 300.0, prob = 0.50 }
      ]
      (intervals [y8, y7, y1, y2])

    , test "Given many layers, should work out unique intervals" <|
      assertSameIntervals
      [ { lower = 20.0, upper = 50.0, prob = 0.15 }
      , { lower = 50.0, upper = 85.0, prob = 0.25 }
      , { lower = 85.0, upper = 110.0, prob = 0.10 }
      , { lower = 110.0, upper = 200.0, prob = 0.40 }
      , { lower = 200.0, upper = 300.0, prob = 0.10 }
      ]
      (intervals [ y3, y2, y6, y8, y1, y4, y5, y7 ])

    ]

rangeTest : Test
rangeTest =
    suite "range'Test"

    [ test "No intervals should give no range" <|
      assertEqual
      Nothing
      (range [])

    , test "One interval should give its range" <|
      assertEqual
      (Just (20.0, 50.0))
      (range
        [ { lower = 20.0, upper = 50.0, prob = 0.15 } ]
      )

    , test "Two intervals in order should give the right range" <|
      assertEqual
      (Just (20.0, 75.0))
      (range
        [ { lower = 20.0, upper = 50.0, prob = 0.15 }
        , { lower = 50.0, upper = 75.0, prob = 0.20 }
        ]
      )

    , test "Two intervals in reverse order should give the right range" <|
      assertEqual
      (Just (20.0, 75.0))
      (range
        [ { lower = 50.0, upper = 75.0, prob = 0.20 }
        , { lower = 20.0, upper = 50.0, prob = 0.15 }
        ]
      )

    ]

