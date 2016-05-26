module DistributionTest exposing (all)

import Distribution exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "DistributionTest"
    [ overlapTest
    ]

overlapTest : Test
overlapTest =
    suite "overlapTest"

    [ test "A finite intersection (ordered well) should be recognised okay" <|
      let
          layer1 = Layer { prob = 1.0, limit = AtLeast, value = 7.0 }
          layer2 = Layer { prob = 1.0, limit = AtMost, value = 123.0 }
      in
          assertEqual
          (Closed 7.0 123.0)
          (overlap layer1 layer2)

    ]

