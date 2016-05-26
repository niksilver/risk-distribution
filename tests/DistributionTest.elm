module DistributionTest exposing (all)

import Distribution exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "DistributionTest"
    [ intervalTestOpenClosedRange
    , intervalTestProbability
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
