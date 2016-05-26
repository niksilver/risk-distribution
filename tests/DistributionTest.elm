module DistributionTest exposing (all)

import Distribution exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "DistributionTest"
    [ intervalTest
    ]

closedRange : Interval -> Maybe (Float, Float)
closedRange interval =
    case interval of
        Open -> Nothing
        Closed range -> Just (range.lower, range.upper)

intervalTest : Test
intervalTest =
    suite "intervalTest"

    [ test "From (-- and --) we should recognise a closed interval (1)" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtLeast, value = 7.0 }
          layer2 = Layer { prob = 1.0, limit = AtMost, value = 123.0 }
      in
          assertEqual
          (Just (7.0, 123.0))
          (interval layer1 layer2 |> closedRange)

    , test "From (-- and --) we should recognise a closed interval (2)" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtLeast, value = 3.4 }
          layer2 = Layer { prob = 1.0, limit = AtMost, value = 6.7 }
      in
          assertEqual
          (Just (3.4, 6.7))
          (interval layer1 layer2 |> closedRange)

    , test "From --) and (-- we should recognise a closed interval" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtMost, value = 6.7 }
          layer2 = Layer { prob = 1.0, limit = AtLeast, value = 3.4 }
      in
          assertEqual
          (Just (3.4, 6.7))
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

