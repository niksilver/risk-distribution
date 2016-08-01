module ConstraintsTest exposing (all)

import Constraints exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "ConstraintsTest"
    [ infinityTest
    , baseZoneTest
    , isInsideTest
    , relativeToTest
    , splitOneTest
    , splitTest
    , overlayOnceTestForAdd
    , overlayOnceTestForSubst
    , overlayOnceTestForNoChange
    , overlayOnceTestForRemainderWithAdd
    , overlayOnceTestForRemainderWithSubst
    , overlayOnceTestForRemainderWithNoChange
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

relativeToTest : Test
relativeToTest =
    suite "relativeToTest"

    [ test "Any point relative to nothing is NoRelation" <|
      assertEqual
      NoRelation
      (relativeTo 0 [])

    , test "A point before all others is Before first" <|
      assertEqual
      (Before (Zone 10 11))
      (relativeTo 2 [Zone 10 11, Zone 11 12])

    , test "A point on the edge the start of one is OnEdgeOf it" <|
      assertEqual
      (OnEdgeOf (Zone 11 12))
      (relativeTo 11 [Zone 10 11, Zone 11 12])

    , test "A point inside one is Inside it" <|
      assertEqual
      (Inside (Zone 11 12))
      (relativeTo 11.5 [Zone 10 11, Zone 11 12])

    , test "A point on the LHS of a gap is before the next one" <|
      assertEqual
      (Before (Zone 12 13))
      (relativeTo 11 [Zone 10 11, Zone 12 13])

    , test "A point in the middle of a gap is before the next one" <|
      assertEqual
      (Before (Zone 12 13))
      (relativeTo 11.5 [Zone 10 11, Zone 12 13])

    , test "A point on the RHS of the last zone has NoRelation" <|
      assertEqual
      (NoRelation)
      (relativeTo 13 [Zone 10 11, Zone 12 13])

    , test "A point after of the last zone has NoRelation" <|
      assertEqual
      (NoRelation)
      (relativeTo 14 [Zone 10 11, Zone 12 13])

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

    , test "Splitting before our zones should yield Nothing" <|
      assertEqual
      (Nothing)
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split -25)

    , test "Splitting after our zones should yield Nothing" <|
      assertEqual
      (Nothing)
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split 25)

    , test "Splitting in a middle gap should yield Nothing" <|
      assertEqual
      (Nothing)
      ([Zone -20 -10, Zone -10 0, Zone 5 10, Zone 10 20] |> split 3)

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

overlayOnceTestForAdd : Test
overlayOnceTestForAdd =
    suite "overlayOnceTestForAdd"

    [ test "Overlaying zone well before all others should add it" <|
      assertEqual
      (AddChange (Add 0 (Zone 0 2)))
      (overlayOnce (Zone 0 2) [Zone 5 10, Zone 10 15] |> fst)

    , test "Overlaying zone just before all others should add it" <|
      assertEqual
      (AddChange (Add 0 (Zone 0 5)))
      (overlayOnce (Zone 0 5) [Zone 5 10, Zone 10 15] |> fst)

    , test "Overlaying zone starting before all others but continuing should add the first part of it" <|
      assertEqual
      (AddChange (Add 0 (Zone 0 5)))
      (overlayOnce (Zone 0 6) [Zone 5 10, Zone 10 15] |> fst)

    , test "Overlaying zone on the left of a gap should add it" <|
      assertEqual
      (AddChange (Add 2 (Zone 15 16)))
      (overlayOnce (Zone 15 16) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone in the middle of a gap should add it" <|
      assertEqual
      (AddChange (Add 2 (Zone 16 19)))
      (overlayOnce (Zone 16 19) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone on the right of a gap should add it" <|
      assertEqual
      (AddChange (Add 2 (Zone 19 20)))
      (overlayOnce (Zone 19 20) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone exactly covering a gap should add it" <|
      assertEqual
      (AddChange (Add 2 (Zone 15 20)))
      (overlayOnce (Zone 15 20) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone in a gap and extending beyond should add it" <|
      assertEqual
      (AddChange (Add 2 (Zone 16 20)))
      (overlayOnce (Zone 16 21) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone just after all others should add it" <|
      assertEqual
      (AddChange (Add 3 (Zone 25 30)))
      (overlayOnce (Zone 25 30) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone some time after all others should add it" <|
      assertEqual
      (AddChange (Add 3 (Zone 27 30)))
      (overlayOnce (Zone 27 30) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    ]

overlayOnceTestForSubst : Test
overlayOnceTestForSubst =
    suite "overlayOnceTestForSubst"

    [ test "Overlaying zone which extends to the end of another should be correct" <|
      assertEqual
      (SubstChange (Subst 1 [Zone 10 12, Zone 12 15]))
      (overlayOnce (Zone 12 20) [Zone 0 10, Zone 10 15, Zone 15 20] |> fst)

    , test "Overlaying zone which extends to the middle of another should be correct" <|
      assertEqual
      (SubstChange (Subst 1 [Zone 10 12, Zone 12 15]))
      (overlayOnce (Zone 12 17) [Zone 0 10, Zone 10 15, Zone 15 20] |> fst)

    , test "Overlaying zone which is the right part of another should be correct" <|
      assertEqual
      (SubstChange (Subst 2 [Zone 10 13, Zone 13 15]))
      (overlayOnce (Zone 13 15) [Zone 0 5, Zone 5 10, Zone 10 15] |> fst)

    , test "Overlaying zone which is in the middle of another should be correct" <|
      assertEqual
      (SubstChange (Subst 2 [Zone 10 11, Zone 11 15]))
      (overlayOnce (Zone 11 12) [Zone 0 5, Zone 5 10, Zone 10 15] |> fst)

    ]

overlayOnceTestForNoChange : Test
overlayOnceTestForNoChange =
    suite "overlayOnceTestForNoChange"

    [ test "Overlaying zone which is the left part of another should yield no change" <|
      assertEqual
      (NoChange)
      (overlayOnce (Zone 0 2) [Zone 0 5, Zone 5 10, Zone 10 15] |> fst)

    , test "Overlaying zone which covers another should yield no change" <|
      assertEqual
      (NoChange)
      (overlayOnce (Zone 0 5) [Zone 0 5, Zone 5 10, Zone 10 15] |> fst)

    , test "Overlaying zone which goes from the left of another and extends should yield no change" <|
      assertEqual
      (NoChange)
      (overlayOnce (Zone 0 6) [Zone 0 5, Zone 5 10, Zone 10 15] |> fst)

    ]

overlayOnceTestForRemainderWithAdd : Test
overlayOnceTestForRemainderWithAdd =
    suite "overlayOnceTestForRemainderWithAdd"

    [ test "Overlaying zone well before all others should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 0 2) [Zone 5 10, Zone 10 15] |> snd)

    , test "Overlaying zone just before all others should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 0 5) [Zone 5 10, Zone 10 15] |> snd)

    , test "Overlaying zone starting before all others but continuing should yield correct remainder" <|
      assertEqual
      (Just (Zone 5 6))
      (overlayOnce (Zone 0 6) [Zone 5 10, Zone 10 15] |> snd)

    , test "Overlaying zone on the left of a gap should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 15 16) [Zone 5 10, Zone 10 15, Zone 20 25] |> snd)

    , test "Overlaying zone in the middle of a gap should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 16 19) [Zone 5 10, Zone 10 15, Zone 20 25] |> snd)

    , test "Overlaying zone on the right of a gap should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 19 20) [Zone 5 10, Zone 10 15, Zone 20 25] |> snd)

    , test "Overlaying zone exactly covering a gap should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 15 20) [Zone 5 10, Zone 10 15, Zone 20 25] |> snd)

    , test "Overlaying zone in a gap and extending beyond should yield correct remainder" <|
      assertEqual
      (Just (Zone 20 21))
      (overlayOnce (Zone 16 21) [Zone 5 10, Zone 10 15, Zone 20 25] |> snd)

    , test "Overlaying zone just after all others should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 25 30) [Zone 5 10, Zone 10 15, Zone 20 25] |> snd)

    , test "Overlaying zone some time after all others should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 27 30) [Zone 5 10, Zone 10 15, Zone 20 25] |> snd)

    ]

overlayOnceTestForRemainderWithSubst : Test
overlayOnceTestForRemainderWithSubst =
    suite "overlayOnceTestForRemainderWithSubst"

    [ test "Overlaying zone which extends into another should yield correct remainder" <|
      assertEqual
      (Just (Zone 15 20))
      (overlayOnce (Zone 12 20) [Zone 0 10, Zone 10 15, Zone 15 20] |> snd)

    , test "Overlaying zone which is the right part of another should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 13 15) [Zone 0 5, Zone 5 10, Zone 10 15] |> snd)

    , test "Overlaying zone which is in the middle of another should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 11 12) [Zone 0 5, Zone 5 10, Zone 10 15] |> snd)

    ]

overlayOnceTestForRemainderWithNoChange : Test
overlayOnceTestForRemainderWithNoChange =
    suite "overlayOnceTestForRemainderWithNoChange"

    [ test "Overlaying zone which is the left part of another should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 0 2) [Zone 0 5, Zone 5 10, Zone 10 15] |> snd)

    , test "Overlaying zone which covers another should yield no remainder" <|
      assertEqual
      (Nothing)
      (overlayOnce (Zone 0 5) [Zone 0 5, Zone 5 10, Zone 10 15] |> snd)

    , test "Overlaying zone which goes from the left of another and extends should yield correct remainder" <|
      assertEqual
      (Just (Zone 5 6))
      (overlayOnce (Zone 0 6) [Zone 0 5, Zone 5 10, Zone 10 15] |> snd)

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

    , test "Adding second segment should add it to the end of segment list" <|
      let
          segNew = Segment 40 (Zone -inf 0)
          seg1 = Segment 50 (Zone -10 10)
          seg2 = Segment 100 (Zone -inf inf)
          model = Model [seg1, seg2] [] []
      in
          assertEqual
          [ seg1, seg2, segNew ]
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
