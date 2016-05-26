module DistributionTest exposing (all)

import Distribution exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "DistributionTest"
    [ intervalTestOpenClosedRange
    , intervalTestProbability
    , bestCounterpartTest
    ]

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

    , test "From (-- and (-- we should recognise an open interval" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtLeast, value = 6.7 }
          layer2 = Layer { prob = 1.0, limit = AtLeast, value = 3.4 }
      in
          assertEqual
          (Open)
          (interval layer1 layer2)

    , test "From --) and --) we should recognise an open interval" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtMost, value = 6.7 }
          layer2 = Layer { prob = 1.0, limit = AtMost, value = 3.4 }
      in
          assertEqual
          (Open)
          (interval layer1 layer2)

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

    ]

bestCounterpartTest : Test
bestCounterpartTest =
    suite "bestCounterpartTest" <|
    let
        {- Total picture is...

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
    in
        [ test "Best counterpart for y1 should be y5" <|
          assertEqual
          (Just y5)
          (bestCounterpart y1 layers)

        , test "Best counterpart for y2 should be y6" <|
          assertEqual
          (Just y6)
          (bestCounterpart y2 layers)

        , test "Best counterpart for y3 should be y8" <|
          assertEqual
          (Just y8)
          (bestCounterpart y3 layers)

        , test "Best counterpart for y4 should be y8" <|
          assertEqual
          (Just y8)
          (bestCounterpart y4 layers)

        ]

