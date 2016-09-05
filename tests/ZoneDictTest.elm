module ZoneDictTest exposing (all)

import ZoneDict exposing (..)

import Zone exposing (Zone)
import Constraint exposing (Constraint)
import Derivation exposing (Derivation)

import ElmTest exposing (..)

all : Test
all =
    suite "ZoneDictTest"
    [ getEntriesTest
    ]

-- A quick way of creating a Derivation

deriv : List Int -> Int -> List Int -> Derivation
deriv coeffs pc src =
    Derivation (Constraint coeffs pc) src

-- The tests...

getEntriesTest : Test
getEntriesTest =
    suite "getEntriesTest"

    [ test "Given no coeffs we should get no entries" <|
      assertEqual
      []
      (getEntries
        [Zone 0 1]
        (deriv [] 100 [0])
      )

    , test "Given single coeff of 1 we should get one entry (1)" <|
      assertEqual
      [(Zone 5 7, Exactly 50 [3])]
      (getEntries
        [Zone 5 7]
        (deriv [1] 50 [3])
      )

    , test "Given single coeff of 1 we should get one entry (2)" <|
      assertEqual
      [(Zone 1 4, Exactly 66 [2])]
      (getEntries
        [Zone 1 4]
        (deriv [1] 66 [2])
      )

    , test "Given single coeff of 1 we should get no entries" <|
      assertEqual
      []
      (getEntries
        [Zone 5 7]
        (deriv [0] 50 [1])
      )

    , test "Given two coeffs of 0, 0 we should get no entries" <|
      assertEqual
      []
      (getEntries
        [Zone 5 7]
        (deriv [0, 0] 50 [2, 0])
      )

    , test "Given two coeffs of 1, 0 we should get entry for first only" <|
      assertEqual
      [(Zone 5 7, Exactly 30 [2, 1])]
      (getEntries
        [Zone 5 7, Zone 7 10]
        (deriv [1, 0] 30 [2, 1])
      )

    , test "Given two coeffs of 0, 1 we should get entry for second only" <|
      assertEqual
      [(Zone 7 10, Exactly 40 [4, 3])]
      (getEntries
        [Zone 5 7, Zone 7 10]
        (deriv [0, 1] 40 [4, 3])
      )

    , test "Given two coeffs of 1, 1 we should get entry for both, stating a max" <|
      assertEqual
      [(Zone 5 7, Maximum 40 [5, 2]), (Zone 7 10, Maximum 40 [5, 2])]
      (getEntries
        [Zone 5 7, Zone 7 10]
        (deriv [1, 1] 40 [5, 2])
      )

    , test "Given three coeffs of 0, 1, 1 we should get entries for second and third only, stating max" <|
      assertEqual
      [(Zone 5 7, Maximum 50 [1, 3]), (Zone 7 10, Maximum 50 [1, 3])]
      (getEntries
        [Zone 0 5, Zone 5 7, Zone 7 10]
        (deriv [0, 1, 1] 50 [1, 3])
      )

    ]
