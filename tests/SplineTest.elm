module SplineTest exposing (all)

import Spline exposing (..)

import ElmTest exposing (..)


all : Test
all =
    suite "SplineTest"
    [ yMinMaxTest
    ]

yMinMaxTest : Test
yMinMaxTest =
    suite "yMinMaxTest"

    [ test "With empty list should yield Nothing" <|
      assertEqual
      (Nothing)
      (yMinMax [])

    , test "With one pos should yield the y value twice" <|
      assertEqual
      (Just (5, 5))
      (yMinMax [Pos 3 5])

    , test "With three poses should yield the correct two y values" <|
      assertEqual
      (Just (2, 6))
      (yMinMax [Pos 3 5, Pos 1 6, Pos 9 2])

    ]
