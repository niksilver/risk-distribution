module ConstraintsTest exposing (all)

import Constraints exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "ConstraintsTest"
    [ infinityTest
    , baseZoneTest
    , relativeToTest
    , splitOneTest
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

    , test "-inf <= -inf" <|
      assert
      (-inf <= -inf)

    , test "inf <= inf" <|
      assert
      (inf <= inf)
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

splitOneTest : Test
splitOneTest =
    suite "splitOneTest"

    [ test "10 should split the base zone (-inf / +inf) correctly" <|
      assertEqual
      ([Zone -inf 10, Zone 10 inf])
      (baseZone |> splitOne 10)

    , test "10 should split (0, 11) correctly" <|
      assertEqual
      ([Zone 0 10, Zone 10 11])
      (Zone 0 11 |> splitOne 10)

    , test "2 should not split (-5, 0) because it's beyond it" <|
      assertEqual
      ([Zone -5 0])
      (Zone -5 0 |> splitOne 2)

    , test "-2 should not split (0, 5) because it's below it" <|
      assertEqual
      ([Zone 0 5])
      (Zone 0 5 |> splitOne -2)

    , test "-1 should not split (-1, 5) because it's on the 'from' edge" <|
      assertEqual
      ([Zone -1 5])
      (Zone -1 5 |> splitOne -1)

    , test "1 should not split (-5, 1) because it's on the 'to' edge" <|
      assertEqual
      ([Zone -5 1])
      (Zone -5 1 |> splitOne 1)

    ]
