module SegmentTest exposing (all)

import Segment exposing (..)

import Constraint exposing (Constraint)
import Zone exposing (Zone)

import ElmTest exposing (..)


all : Test
all =
    suite "SegmentTest"
    [ constraintTest
    ]

constraintTest : Test
constraintTest =
    suite "constraintTest"

    [ test "A constraint at the start should work" <|
      assertEqual
      (Constraint [1, 1, 0] 45)
      (constraint
        (Segment 45 (Zone -10 10))
        [Zone -10 0, Zone 0 10, Zone 10 20]
      )

    , test "A constraint in the middle should work" <|
      assertEqual
      (Constraint [0, 1, 1, 0] 22)
      (constraint
        (Segment 22 (Zone 0 15))
        [Zone -10 0, Zone 0 10, Zone 10 15, Zone 15 20]
      )

    , test "A constraint at the end should work" <|
      assertEqual
      (Constraint [0, 0, 1, 1] 33)
      (constraint
        (Segment 33 (Zone 10 20))
        [Zone -10 0, Zone 0 10, Zone 10 15, Zone 15 20]
      )

    , test "A constraint covering one zone on the left should work" <|
      assertEqual
      (Constraint [1, 0, 0, 0] 44)
      (constraint
        (Segment 44 (Zone -10 0))
        [Zone -10 0, Zone 0 10, Zone 10 15, Zone 15 20]
      )

    , test "A constraint covering one zone on the right should work" <|
      assertEqual
      (Constraint [0, 0, 0, 1] 55)
      (constraint
        (Segment 55 (Zone 15 20))
        [Zone -10 0, Zone 0 10, Zone 10 15, Zone 15 20]
      )

    , test "A constraint covering all zones should work" <|
      assertEqual
      (Constraint [1, 1, 1, 1] 66)
      (constraint
        (Segment 66 (Zone -10 20))
        [Zone -10 0, Zone 0 10, Zone 10 15, Zone 15 20]
      )

    ]
