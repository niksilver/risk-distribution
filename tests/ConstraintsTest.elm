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
    , splitTest
    , constraintToStringTest
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

splitTest : Test
splitTest =
    suite "splitTest"

    [ test "Splitting no zones should give no zones" <|
      assertEqual
      (Nothing)
      ([] |> split 10)

    , test "Splitting base zone should give two zones substituting" <|
      assertEqual
      (Just (Subst 0 [Zone -inf 11, Zone 11 inf]))
      ([baseZone] |> split 11)

    , test "Splitting a first zone should give two zones substituting" <|
      assertEqual
      (Just(Subst 0 [Zone -inf -12, Zone -12 0]))
      ([Zone -inf 0, Zone 0 inf] |> split -12)

    , test "Splitting a second zone should give two zones substituting" <|
      assertEqual
      (Just (Subst 1 [Zone -10 12, Zone 12 inf]))
      ([Zone -inf -10, Zone -10 inf] |> split 12)

    , test "Splitting a third zone should give two zones substituting" <|
      assertEqual
      (Just (Subst 2 [Zone 10 13, Zone 13 inf]))
      ([Zone -inf -10, Zone -10 10, Zone 10 inf] |> split 13)

    , test "Splitting outside our zones should yield Nothing" <|
      assertEqual
      (Nothing)
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split 25)

    , test "Splitting on the lowest edge should yield Nothing" <|
      assertEqual
      (Nothing)
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split -20)

    , test "Splitting on the highest edge should yield Nothing" <|
      assertEqual
      (Nothing)
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split 20)

    , test "Splitting on a middle edge should yield Nothing" <|
      assertEqual
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split 10)
      (Nothing)

    ]

constraintToStringTest : Test
constraintToStringTest =
    suite "constraintToStringTest"

    [ test "Should work for 'a = 40' (with just a known)" <|
      assertEqual
      "a = 40"
      (Constraint [1] 40 |> constraintToString)

    , test "Should work for 'a = 40' (with a and b known)" <|
      assertEqual
--     a + b = ..
      "a     = 40"
      (Constraint [1, 0] 40 |> constraintToString)

    , test "Should work for 'a = 40' (with a, b and c known)" <|
      assertEqual
--     a + b + c = ..
      "a         = 40"
      (Constraint [1, 0, 0] 40 |> constraintToString)

    , test "Should work for 'a + b = 60'" <|
      assertEqual
--     a + b + c = ..
      "a + b     = 60"
      (Constraint [1, 1, 0] 60 |> constraintToString)

    , test "Should work for 'a + c = 33'" <|
      assertEqual
--     a + b + c = ..
      "a     + c = 33"
      (Constraint [1, 0, 1] 33 |> constraintToString)

    , test "Should work for 'a + b + c = 65'" <|
      assertEqual
      "a + b + c = 65"
      (Constraint [1, 1, 1] 65 |> constraintToString)

    , test "Should work for 'b = 10'" <|
      assertEqual
--     a + b + c = ..
      "    b     = 10"
      (Constraint [0, 1, 0] 10 |> constraintToString)

    , test "Should work for 'b + c = 15'" <|
      assertEqual
--     a + b + c = ..
      "    b + c = 15"
      (Constraint [0, 1, 1] 15 |> constraintToString)

    , test "Should work for 'c = 4'" <|
      assertEqual
--     a + b + c = ..
      "        c = 4"
      (Constraint [0, 0, 1] 4 |> constraintToString)

    ]
