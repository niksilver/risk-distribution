module AxisTest exposing (all)

import Axis exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "AxisTest"
    [ scaleTest
    ]

scaleTest : Test
scaleTest =
    suite "scaleTest"

    [ test "Axis 1 to 10 with max 5 ticks should give 0 to 10 step 2" <|
      assertEqual
      { min = 0, max = 10, step = 2 }
      (scale 1 10 5)

    , test "Axis 2 to 10 with max 5 ticks should give 2 to 10 step 2" <|
      assertEqual
      { min = 2, max = 10, step = 2 }
      (scale 2 10 5)

    , test "Axis 15 to 234 with max 9 ticks should give 0 to 250 step 50" <|
      assertEqual
      { min = 0, max = 250, step = 50 }
      (scale 15 234 9)

    , test "Axis 1 to 14 with max 4 ticks should give 0 to 15 step 5" <|
      assertEqual
      { min = 0, max = 15, step = 5 }
      (scale 1 14 4)

    , test "Axis -11 to 3 with max 4 ticks should give -15 to 5 step 5" <|
      assertEqual
      { min = -15, max = 5, step = 5 }
      (scale -11 3 4)

    , test "Axis -0.085 to 0.173 with max 10 ticks should give -0.1 to 0.2 step 0.05" <|
      assertEqual
      { min = -0.1, max = 0.2, step = 0.05 }
      (scale -0.085 0.173 10)

    ]
