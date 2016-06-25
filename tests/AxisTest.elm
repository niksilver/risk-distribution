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

    [ test "Axis 1 to 10 with 5 ticks should give 0 to 12 step 3" <|
      assertEqual
      { min = 0, max = 12, step = 3 }
      (scale 1 10 5)

    , test "Axis 2 to 10 with 5 ticks should give 2 to 10 step 2" <|
      assertEqual
      { min = 2, max = 10, step = 2 }
      (scale 2 10 5)

    , test "Axis 15 to 234 with 9 ticks should give 0 to 240 step 30" <|
      assertEqual
      { min = 0, max = 240, step = 30 }
      (scale 15 234 9)

    ]
