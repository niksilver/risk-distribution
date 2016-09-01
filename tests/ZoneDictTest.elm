module ZoneDictTest exposing (all)

import ZoneDict exposing (..)

import Zone exposing (Zone)
import Constraint exposing (Constraint)

import ElmTest exposing (..)

all : Test
all =
    suite "ZoneDictTest"
    [ getEntriesTest
    ]

-- The tests...

getEntriesTest : Test
getEntriesTest =
    suite "getEntriesTest"

    [ test "Given no coeffs we should get no entries" <|
      assertEqual
      []
      (getEntries
        [Zone 0 1]
        (Constraint [] 100)
      )

    , test "Given single coeff of 1 we should get one entry (1)" <|
      assertEqual
      [(Zone 5 7, Exactly 50)]
      (getEntries
        [Zone 5 7]
        (Constraint [1] 50)
      )

    , test "Given single coeff of 1 we should get one entry (2)" <|
      assertEqual
      [(Zone 1 4, Exactly 66)]
      (getEntries
        [Zone 1 4]
        (Constraint [1] 66)
      )

    , test "Given single coeff of 1 we should get no entries" <|
      assertEqual
      []
      (getEntries
        [Zone 5 7]
        (Constraint [0] 50)
      )

    , test "Given two coeffs of 0, 0 we should get no entries" <|
      assertEqual
      []
      (getEntries
        [Zone 5 7]
        (Constraint [0, 0] 50)
      )

    , test "Given two coeffs of 1, 0 we should get entry for first only" <|
      assertEqual
      [(Zone 5 7, Exactly 30)]
      (getEntries
        [Zone 5 7, Zone 7 10]
        (Constraint [1, 0] 30)
      )

    , test "Given two coeffs of 0, 1 we should get entry for second only" <|
      assertEqual
      [(Zone 7 10, Exactly 40)]
      (getEntries
        [Zone 5 7, Zone 7 10]
        (Constraint [0, 1] 40)
      )

    , test "Given two coeffs of 1, 1 we should get entry for both, stating a max" <|
      assertEqual
      [(Zone 5 7, Maximum 40), (Zone 7 10, Maximum 40)]
      (getEntries
        [Zone 5 7, Zone 7 10]
        (Constraint [1, 1] 40)
      )

    , test "Given three coeffs of 0, 1, 1 we should get entris for second and third only, stating max" <|
      assertEqual
      [(Zone 5 7, Maximum 50), (Zone 7 10, Maximum 50)]
      (getEntries
        [Zone 0 5, Zone 5 7, Zone 7 10]
        (Constraint [0, 1, 1] 50)
      )

    ]
