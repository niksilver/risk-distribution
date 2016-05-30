module DistributionTest exposing (all)

import Distribution exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "DistributionTest"
    [ intervalTestOpenClosedRange
    , intervalTestProbability
    , bestGreaterCounterpartTest
    , bestLesserCounterpartTest
    , sortIntervalsTest
    , intervalsTest
    ]

 
-- Utilities -----------------------------------------------


assertSameIntervals : List Interval -> List Interval -> Assertion
assertSameIntervals x1 x2 =
    let
        x1Sorted = sortIntervals x1
        x2Sorted = sortIntervals x2
    in
        assertEqual x1Sorted x2Sorted

closedRange : Interval -> Maybe (Float, Float)
closedRange interval =
    case interval of
        Open -> Nothing
        Closed desc -> Just (desc.lower, desc.upper)

probability : Interval -> Maybe Float
probability interval =
    case interval of
        Open -> Nothing
        Closed desc -> Just desc.prob


-- Tests ----------------------------------------------------


intervalTestOpenClosedRange : Test
intervalTestOpenClosedRange =
    suite "intervalTest - open/closed range"

    [ test "From (-- and --) overlapping we should recognise a closed interval (1)" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtLeast, value = 7.0 }
          layer2 = Layer { prob = 1.0, limit = AtMost, value = 123.0 }
      in
          assertEqual
          (Just (7.0, 123.0))
          (interval layer1 layer2 |> closedRange)

    , test "From (-- and --) overlapping we should recognise a closed interval (2)" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtLeast, value = 3.4 }
          layer2 = Layer { prob = 1.0, limit = AtMost, value = 6.7 }
      in
          assertEqual
          (Just (3.4, 6.7))
          (interval layer1 layer2 |> closedRange)

    , test "From --) and (-- overlapping we should recognise a closed interval" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtMost, value = 6.7 }
          layer2 = Layer { prob = 1.0, limit = AtLeast, value = 3.4 }
      in
          assertEqual
          (Just (3.4, 6.7))
          (interval layer1 layer2 |> closedRange)

    , test "From --) and (-- not overlapping we should recognise a closed interval" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtMost, value = 55.0 }
          layer2 = Layer { prob = 1.0, limit = AtLeast, value = 66.0 }
      in
          assertEqual
          (Just (55.0, 66.0))
          (interval layer1 layer2 |> closedRange)

    , test "From early (-- and later (-- we should recognise a closed interval" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtLeast, value = 3.4 }
          layer2 = Layer { prob = 0.8, limit = AtLeast, value = 6.7 }
      in
          assertEqual
          (Just (3.4, 6.7))
          (interval layer1 layer2 |> closedRange)

    , test "From late (-- and earlier (-- we should recognise a closed interval" <|
      let
          layer1 = Layer { prob = 0.75, limit = AtLeast, value = 6.7 }
          layer2 = Layer { prob = 1.0, limit = AtLeast, value = 3.4 }
      in
          assertEqual
          (Just (3.4, 6.7))
          (interval layer1 layer2 |> closedRange)

    , test "From early --) and later --) we should recognise a closed interval" <|
      let
          layer1 = Layer { prob = 0.6, limit = AtMost, value = 3.3 }
          layer2 = Layer { prob = 1.0, limit = AtMost, value = 7.7 }
      in
          assertEqual
          (Just (3.3, 7.7))
          (interval layer1 layer2 |> closedRange)

    , test "From late --) and earlier --) we should recognise a closed interval" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtMost, value = 6.6 }
          layer2 = Layer { prob = 0.3, limit = AtMost, value = 3.2 }
      in
          assertEqual
          (Just (3.2, 6.6))
          (interval layer1 layer2 |> closedRange)

    ]

intervalTestProbability : Test
intervalTestProbability =
    suite "intervalTest - probability"

    [ test "From (-- and --) overlapping we should get the right probability" <|
      let
          layer1 = Layer { prob = 0.50, limit = AtLeast, value = 7.0 }
          layer2 = Layer { prob = 0.70, limit = AtMost, value = 123.0 }
      in
          assertEqual
          (Just 0.20)
          (interval layer1 layer2 |> probability)

    , test "From --) and (-- overlapping we should get the right probability" <|
      let
          layer1 = Layer { prob = 0.90, limit = AtMost, value = 123.0 }
          layer2 = Layer { prob = 0.80, limit = AtLeast, value = 7.0 }
      in
          assertEqual
          (Just 0.70)
          (interval layer1 layer2 |> probability)

    , test "From early (-- and later (-- we should get the right probability" <|
      let
          layer1 = Layer { prob = 0.9, limit = AtLeast, value = 3.4 }
          layer2 = Layer { prob = 0.8, limit = AtLeast, value = 6.7 }
      in
          assertEqual
          (Just 0.1)
          (interval layer1 layer2 |> probability)

    , test "From late (-- and earlier (-- we should get the right probability" <|
      let
          layer1 = Layer { prob = 0.7, limit = AtLeast, value = 7.7 }
          layer2 = Layer { prob = 0.9, limit = AtLeast, value = 3.3 }
      in
          assertEqual
          (Just 0.2)
          (interval layer1 layer2 |> probability)

    , test "From early --) and later --) we should get the right probability" <|
      let
          layer1 = Layer { prob = 0.6, limit = AtMost, value = 3.3 }
          layer2 = Layer { prob = 1.0, limit = AtMost, value = 7.7 }
      in
          assertEqual
          (Just 0.4)
          (interval layer1 layer2 |> probability)

    , test "From late --) and earlier --) we should get the right probability" <|
      let
          layer1 = Layer { prob = 0.75, limit = AtMost, value = 8.8 }
          layer2 = Layer { prob = 0.45, limit = AtMost, value = 4.6 }
      in
          assertEqual
          (Just 0.30)
          (interval layer1 layer2 |> probability)

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

y1 = Layer { prob = 1.00, limit = AtLeast, value = 20.0 }
y2 = Layer { prob = 0.85, limit = AtLeast, value = 50.0 }
y3 = Layer { prob = 0.50, limit = AtLeast, value = 110.0 }
y4 = Layer { prob = 0.10, limit = AtLeast, value = 200.0 }
y5 = Layer { prob = 0.15, limit = AtMost, value = 50.0 }
y6 = Layer { prob = 0.40, limit = AtMost, value = 85.0 }
y7 = Layer { prob = 0.50, limit = AtMost, value = 110.0 }
y8 = Layer { prob = 1.00, limit = AtMost, value = 300.0 }
layers = [ y7, y5, y3, y1, y2, y4, y6, y8 ]

bestGreaterCounterpartTest : Test
bestGreaterCounterpartTest =
    suite "bestGreaterCounterpartTest"

    [ test "Best greater counterpart for y1 should be y5" <|
      assertEqual
      (Just y5)
      (bestGreaterCounterpart y1 layers)

    , test "Best greater counterpart for y2 should be y6" <|
      assertEqual
      (Just y6)
      (bestGreaterCounterpart y2 layers)

    , test "Best greater counterpart for y3 should be y8" <|
      assertEqual
      (Just y8)
      (bestGreaterCounterpart y3 layers)

    , test "Best greater counterpart for y4 should be y8" <|
      assertEqual
      (Just y8)
      (bestGreaterCounterpart y4 layers)

    , test "Best greater counterpart for y5 should be y3" <|
      assertEqual
      (Just y3)
      (bestGreaterCounterpart y5 layers)

    , test "Best greater counterpart for y6 should be y3" <|
      assertEqual
      (Just y3)
      (bestGreaterCounterpart y6 layers)

    , test "Best greater counterpart for y7 should be y4" <|
      assertEqual
      (Just y4)
      (bestGreaterCounterpart y7 layers)

    , test "Best greater counterpart for y8 should not exist" <|
      assertEqual
      (Nothing)
      (bestGreaterCounterpart y8 layers)

    ]

bestLesserCounterpartTest : Test
bestLesserCounterpartTest =
    suite "bestLesserCounterpartTest"

    [ test "Best lesser counterpart for y1 should not exist" <|
      assertEqual
      (Nothing)
      (bestLesserCounterpart y1 layers)

    , test "Best lesser counterpart for y2 should not exist" <|
      assertEqual
      (Nothing)
      (bestLesserCounterpart y2 layers)

    , test "Best lesser counterpart for y3 should be y6" <|
      assertEqual
      (Just y6)
      (bestLesserCounterpart y3 layers)

    , test "Best lesser counterpart for y4 should be y7" <|
      assertEqual
      (Just y7)
      (bestLesserCounterpart y4 layers)

    , test "Best lesser counterpart for y5 should be y1" <|
      assertEqual
      (Just y1)
      (bestLesserCounterpart y5 layers)

    , test "Best lesser counterpart for y6 should be y2" <|
      assertEqual
      (Just y2)
      (bestLesserCounterpart y6 layers)

    , test "Best lesser counterpart for y7 should be y2" <|
      assertEqual
      (Just y2)
      (bestLesserCounterpart y7 layers)

    , test "Best lesser counterpart for y8 should be y4" <|
      assertEqual
      (Just y4)
      (bestLesserCounterpart y8 layers)

    ]

sortIntervalsTest : Test
sortIntervalsTest =
    suite "sortIntervalsTest" <|

        {- Intervals in this suite:
        
              1.0  2.0  3.0  4.0  5.0  6.0  7.0
               |----|----|----|----|----|----|
           i1  (---------)
           i2       (----)
           i3            (---------)
           i4       (---------)
           i5            (----)
           i6       (-----------
        -}

        let
            i1 = Closed { lower = 1.0, upper = 3.0, prob = 1.0 }
            i2 = Closed { lower = 2.0, upper = 3.0, prob = 1.0 }
            i3 = Closed { lower = 3.0, upper = 5.0, prob = 1.0 }
            i4 = Closed { lower = 2.0, upper = 4.0, prob = 1.0 }
            i5 = Closed { lower = 3.0, upper = 4.0, prob = 1.0 }
            i6 = Open
        in
            [ test "Empty list sorted should be empty" <|
              assertEqual
              []
              (sortIntervals [])

            , test ("Intervals with all different lower bounds "
                ++ "should be sorted by lower bounds") <|
              assertEqual
              [i1, i2, i3]
              (sortIntervals [i3, i2, i1])

            , test ("Intervals with all same lower bounds "
                ++ "should be sorted by lower then upper bounds") <|
              assertEqual
              [i2, i4, i5, i3]
              (sortIntervals [i5, i4, i3, i2])

            , test "Open intervals should be ordered last" <|
              assertEqual
              [i1, i4, i5, i6]
              (sortIntervals [i5, i4, i6, i1])

            ]

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
      [ Closed { lower = 20.0, upper = 50.0, prob = 0.15 } ]
      (intervals [y1, y5])

    , test "Given two intersecting layers reversed, should work out single interval" <|
      assertEqual
      [ Closed { lower = 20.0, upper = 50.0, prob = 0.15 } ]
      (intervals [y5, y1])

    , test "Given two layers in same direction, should work out single interval" <|
      assertEqual
      [ Closed { lower = 20.0, upper = 110.0, prob = 0.50 } ]
      (intervals [y1, y3])

    , test "Given three layers in same direction, should work out two intervals" <|
      assertSameIntervals
      [ Closed { lower = 20.0, upper = 50.0, prob = 0.15 }
      , Closed { lower = 50.0, upper = 110.0, prob = 0.35 }
      ]
      (intervals [y1, y2, y3])

    ]

