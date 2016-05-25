module DistributionTest exposing (all)

import ElmTest exposing (..)

all : Test
all =
    suite "DistributionTest"
    [ dummyTest
    ]

dummyTest : Test
dummyTest =
    suite "dummyTest"

    [ test "Dummy test one" <|
      assertEqual
      "Hello"
      ("H" ++ "ello")

    ]

