module ConstraintsTest exposing (all)

import Constraints exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "ConstraintsTest"
    [ infinityTest
    , baseZoneTest
    , relativeToTest
    ]

infinityTest : Test
infinityTest =
    suite "infinityTest"
    [ test "inf is very, very positive" <|
      assert
      (100000 < inf)

    , test "-inf is very, very negative" <|
      assert
      (-inf < -100000)

    , test "-inf is less than inf" <|
      assert
      (-inf < inf)
    ]

baseZoneTest : Test
baseZoneTest =
    suite "baseZoneTest"
    [ test "Baze zone runs from -inf" <|
      assertEqual
      -inf
      baseZone.from

    , test "Baze zone runs to +inf" <|
      assertEqual
      inf
      baseZone.to

    ]

relativeToTest : Test
relativeToTest =
    suite "relativeToTest"
    [ test "-11 is outside -10 to +10" <|
      assertEqual
      Outside
      (Zone -10 10 |> relativeTo -11)

    , test "11 is outside -10 to +10" <|
      assertEqual
      Outside
      (Zone -10 10 |> relativeTo 11)

    , test "4 is inside 0 to +5" <|
      assertEqual
      Inside
      (Zone 0 5 |> relativeTo 4)

    , test "-1.5 is on the 'from' edge of -1.5 to 0" <|
      assertEqual
      (Edge)
      (Zone -1.5 0 |> relativeTo -1.5)

    , test "2.5 is on the 'to' edge of -1.5 to 2.5" <|
      assertEqual
      (Edge)
      (Zone -1.5 2.5 |> relativeTo 2.5)

    ]
