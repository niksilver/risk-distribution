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

    ]
