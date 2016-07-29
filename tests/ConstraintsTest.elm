module ConstraintsTest exposing (all)

import Constraints exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "ConstraintsTest"
    [ infinityTest
    , baseZoneTest
    , isInsideTest
    , splitOneTest
    , splitTest
    , constraintToStringTest
    , addSegmentTestForNewSegment
    , addSegmentTestForNewZone
    , addSegmentTestForNewVariables
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

isInsideTest : Test
isInsideTest =
    suite "isInsideTest"
    [ test "-11 is outside -10 to +10" <|
      assertEqual
      False
      (Zone -10 10 |> isInside -11)

    , test "11 is outside -10 to +10" <|
      assertEqual
      False
      (Zone -10 10 |> isInside 11)

    , test "4 is inside 0 to +5" <|
      assertEqual
      True
      (Zone 0 5 |> isInside 4)

    , test "-1.5 is on the 'from' edge of -1.5 to 0, so outside" <|
      assertEqual
      False
      (Zone -1.5 0 |> isInside -1.5)

    , test "2.5 is on the 'to' edge of -1.5 to 2.5, so outside" <|
      assertEqual
      False
      (Zone -1.5 2.5 |> isInside 2.5)

    ]

splitOneTest : Test
splitOneTest =
    suite "splitOneTest"

    [ test "10 should split the base zone (-inf / +inf) correctly" <|
      assertEqual
      (Just [Zone -inf 10, Zone 10 inf])
      (baseZone |> splitOne 10)

    , test "10 should split (0, 11) correctly" <|
      assertEqual
      (Just [Zone 0 10, Zone 10 11])
      (Zone 0 11 |> splitOne 10)

    , test "2 should not split (-5, 0) because it's beyond it" <|
      assertEqual
      (Nothing)
      (Zone -5 0 |> splitOne 2)

    , test "-2 should not split (0, 5) because it's below it" <|
      assertEqual
      (Nothing)
      (Zone 0 5 |> splitOne -2)

    , test "-1 should not split (-1, 5) because it's on the 'from' edge" <|
      assertEqual
      (Nothing)
      (Zone -1 5 |> splitOne -1)

    , test "1 should not split (-5, 1) because it's on the 'to' edge" <|
      assertEqual
      (Nothing)
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

addSegmentTestForNewSegment : Test
addSegmentTestForNewSegment =
    suite "addSegmentTestForNewSegment"

    [ test "Adding first segment should add it okay" <|
      let
          segment = Segment 40 (Zone -inf 0)
          model = Model [] [] []
      in
          assertEqual
          [ segment ]
          (addSegment segment model |> .segments)

    , test "Adding second segment should add it to the start of segment list" <|
      let
          segNew = Segment 40 (Zone -inf 0)
          seg1 = Segment 50 (Zone -10 10)
          seg2 = Segment 100 (Zone -inf inf)
          model = Model [seg1, seg2] [] []
      in
          assertEqual
          [ segNew, seg1, seg2 ]
          (addSegment segNew model |> .segments)

    ]

addSegmentTestForNewZone : Test
addSegmentTestForNewZone =
    suite "addSegmentTestForNewZone"

    [ test "Adding segment that splits one zone with its 'from' should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 25 30)
          model = Model [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, Zone 20 25, Zone 25 30]
          (addSegment seg model |> .zones)

    , test "Adding segment that splits one zone with its 'to' should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 10 15)
          model = Model [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, Zone 10 15, Zone 15 20, zone3]
          (addSegment seg model |> .zones)

    , test "Adding segment that splits one zone into three should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 5 6)
          model = Model [] [zone1, zone2, zone3] []
      in
          assertEqual
          [Zone 0 5, Zone 5 6, Zone 6 10, zone2, zone3]
          (addSegment seg model |> .zones)

    , test "Adding segment that splits two neighbouring zones should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 50 (Zone 15 25)
          model = Model [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, Zone 10 15, Zone 15 20, Zone 20 25, Zone 25 30]
          (addSegment seg model |> .zones)

    , test "Adding segment that splits two distant zones should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 65 (Zone 9 21)
          model = Model [] [zone1, zone2, zone3] []
      in
          assertEqual
          [Zone 0 9, Zone 9 10, zone2, Zone 20 21, Zone 21 30]
          (addSegment seg model |> .zones)

    , test "Adding segment that is outsize zones should not change zones" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 30 40)
          model = Model [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, zone3]
          (addSegment seg model |> .zones)

    , test "Adding segment that splits no zones should not change zones" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 10 30)
          model = Model [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, zone3]
          (addSegment seg model |> .zones)

    ]

addSegmentTestForNewVariables : Test
addSegmentTestForNewVariables =
    suite "addSegmentTestForNewVariables"

    [ test "Adding segment that substitutes one var should be okay" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          -- This Segment substitutes the last variable only
          seg = Segment 40 (Zone 25 30)
          con1 = Constraint [1, 1, 1] 100
          con2 = Constraint [1, 1, 0] 40
          con3 = Constraint [0, 1, 1] 50
          model = Model [] [zone1, zone2, zone3] [con1, con2, con3]
      in
          assertEqual
          [ Constraint [1, 1, 1, 1] 100
          , Constraint [1, 1, 0, 0] 40
          , Constraint [0, 1, 1, 1] 50
          ]
          (addSegment seg model |> .constraints)

    , test "Adding segment that substitutes two vars should be okay" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          -- This Segment should split first and third var
          seg = Segment 65 (Zone 9 21)
          con1 = Constraint [1, 1, 1] 100
          con2 = Constraint [1, 1, 0] 40
          con3 = Constraint [0, 1, 1] 50
          model = Model [] [zone1, zone2, zone3] [con1, con2, con3]
      in
          assertEqual
          [ Constraint [1, 1, 1, 1, 1] 100
          , Constraint [1, 1, 1, 0, 0] 40
          , Constraint [0, 0, 1, 1, 1] 50
          ]
          (addSegment seg model |> .constraints)

    , test "Adding segment that substitutes no vars should be okay" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          -- This Segment should split no vars
          seg = Segment 75 (Zone 10 30)
          con1 = Constraint [1, 1, 1] 100
          con2 = Constraint [1, 1, 0] 40
          con3 = Constraint [0, 1, 1] 50
          model = Model [] [zone1, zone2, zone3] [con1, con2, con3]
      in
          assertEqual
          [con1, con2, con3]
          (addSegment seg model |> .constraints)

    ]
