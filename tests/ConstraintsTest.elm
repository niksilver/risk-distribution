module ConstraintsTest exposing (all)

import Constraints exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "ConstraintsTest"
    [ infinityTest
    , baseZoneTest
    , isSubzoneTest
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
    , overlayTest
    , applyTest
    , constraintToStringTest
    , addSegmentTestForNewSegment
    , addSegmentTestForNewZone
    , addSegmentTestForNewVariables
    , applyToCoeffsTest
    , constraintTest
    , addConstraintTest
    , isSubcoeffTest
    , subtractTest
    , deriveOnceTest
    , deriveAllTest
    , modelTest
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

isSubzoneTest : Test
isSubzoneTest =
    suite "isSubzoneTest"

    [ test "Zone some way to the left of is not a subzone" <|
      assertEqual
      False
      (isSubzone (Zone 0 2) (Zone 5 10))

    , test "Zone immediately to the left of is not a subzone" <|
      assertEqual
      False
      (isSubzone (Zone 4 5) (Zone 5 10))

    , test "Zone overlapping left of is not a subzone" <|
      assertEqual
      False
      (isSubzone (Zone 5 7) (Zone 6 12))

    , test "Zone in and on the left is a subzone" <|
      assertEqual
      True
      (isSubzone (Zone 6 8) (Zone 6 12))

    , test "Zone in the middle of is a subzone" <|
      assertEqual
      True
      (isSubzone (Zone 17 18) (Zone 16 20))

    , test "Zone in and on the right is a subzone" <|
      assertEqual
      True
      (isSubzone (Zone 10 12) (Zone 6 12))

    , test "Zone is a subzone of itself" <|
      assertEqual
      True
      (isSubzone (Zone 5 13) (Zone 5 13))

    , test "Large zone is not a subzone of a contained zone" <|
      assertEqual
      False
      (isSubzone (Zone 4 14) (Zone 5 13))

    , test "Zone overlapping right of is not a subzone" <|
      assertEqual
      False
      (isSubzone (Zone 10 14) (Zone 6 12))

    , test "Zone immediately to the right of is not a subzone" <|
      assertEqual
      False
      (isSubzone (Zone 25 26) (Zone 20 25))

    , test "Zone some way to the right of is not a subzone" <|
      assertEqual
      False
      (isSubzone (Zone 15 77) (Zone 6 12))

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
      (NoChange)
      ([] |> split 10)

    , test "Splitting base zone should give two zones substituting" <|
      assertEqual
      (Subst 0 [Zone -inf 11, Zone 11 inf])
      ([baseZone] |> split 11)

    , test "Splitting a first zone should give two zones substituting" <|
      assertEqual
      (Subst 0 [Zone -inf -12, Zone -12 0])
      ([Zone -inf 0, Zone 0 inf] |> split -12)

    , test "Splitting a second zone should give two zones substituting" <|
      assertEqual
      (Subst 1 [Zone -10 12, Zone 12 inf])
      ([Zone -inf -10, Zone -10 inf] |> split 12)

    , test "Splitting a third zone should give two zones substituting" <|
      assertEqual
      (Subst 2 [Zone 10 13, Zone 13 inf])
      ([Zone -inf -10, Zone -10 10, Zone 10 inf] |> split 13)

    , test "Splitting before our zones should yield Nothing" <|
      assertEqual
      (NoChange)
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split -25)

    , test "Splitting after our zones should yield Nothing" <|
      assertEqual
      (NoChange)
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split 25)

    , test "Splitting in a middle gap should yield Nothing" <|
      assertEqual
      (NoChange)
      ([Zone -20 -10, Zone -10 0, Zone 5 10, Zone 10 20] |> split 3)

    , test "Splitting on the lowest edge should yield Nothing" <|
      assertEqual
      (NoChange)
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split -20)

    , test "Splitting on the highest edge should yield Nothing" <|
      assertEqual
      (NoChange)
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split 20)

    , test "Splitting on a middle edge should yield Nothing" <|
      assertEqual
      ([Zone -20 -10, Zone -10 10, Zone 10 20] |> split 10)
      (NoChange)

    ]

overlayOnceTestForAdd : Test
overlayOnceTestForAdd =
    suite "overlayOnceTestForAdd"

    [ test "Overlaying zone well before all others should add it" <|
      assertEqual
      (Add 0 (Zone 0 2))
      (overlayOnce (Zone 0 2) [Zone 5 10, Zone 10 15] |> fst)

    , test "Overlaying zone just before all others should add it" <|
      assertEqual
      (Add 0 (Zone 0 5))
      (overlayOnce (Zone 0 5) [Zone 5 10, Zone 10 15] |> fst)

    , test "Overlaying zone starting before all others but continuing should add the first part of it" <|
      assertEqual
      (Add 0 (Zone 0 5))
      (overlayOnce (Zone 0 6) [Zone 5 10, Zone 10 15] |> fst)

    , test "Overlaying zone on the left of a gap should add it" <|
      assertEqual
      (Add 2 (Zone 15 16))
      (overlayOnce (Zone 15 16) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone in the middle of a gap should add it" <|
      assertEqual
      (Add 2 (Zone 16 19))
      (overlayOnce (Zone 16 19) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone on the right of a gap should add it" <|
      assertEqual
      (Add 2 (Zone 19 20))
      (overlayOnce (Zone 19 20) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone exactly covering a gap should add it" <|
      assertEqual
      (Add 2 (Zone 15 20))
      (overlayOnce (Zone 15 20) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone in a gap and extending beyond should add it" <|
      assertEqual
      (Add 2 (Zone 16 20))
      (overlayOnce (Zone 16 21) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone just after all others should add it" <|
      assertEqual
      (Add 3 (Zone 25 30))
      (overlayOnce (Zone 25 30) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    , test "Overlaying zone some time after all others should add it" <|
      assertEqual
      (Add 3 (Zone 27 30))
      (overlayOnce (Zone 27 30) [Zone 5 10, Zone 10 15, Zone 20 25] |> fst)

    ]

overlayOnceTestForSubst : Test
overlayOnceTestForSubst =
    suite "overlayOnceTestForSubst"

    [ test "Overlaying zone which extends to the end of another should be correct" <|
      assertEqual
      (Subst 1 [Zone 10 12, Zone 12 15])
      (overlayOnce (Zone 12 20) [Zone 0 10, Zone 10 15, Zone 15 20] |> fst)

    , test "Overlaying zone which extends to the middle of another should be correct" <|
      assertEqual
      (Subst 1 [Zone 10 12, Zone 12 15])
      (overlayOnce (Zone 12 17) [Zone 0 10, Zone 10 15, Zone 15 20] |> fst)

    , test "Overlaying zone which is the right part of another should be correct" <|
      assertEqual
      (Subst 2 [Zone 10 13, Zone 13 15])
      (overlayOnce (Zone 13 15) [Zone 0 5, Zone 5 10, Zone 10 15] |> fst)

    , test "Overlaying zone which is in the middle of another should be correct" <|
      assertEqual
      (Subst 2 [Zone 10 11, Zone 11 15])
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

overlayTest : Test
overlayTest =
    suite "overlayTest"

    [ test "Overlaying to the left of some zones should add it" <|
      assertEqual
      [Add 0 (Zone -10 -5)]
      (overlay (Zone -10 -5) [Zone 0 1, Zone 1 2, Zone 2 3])

    , test "Overlaying to the right of some zones should add it" <|
      assertEqual
      [Add 3 (Zone 10 11)]
      (overlay (Zone 10 11) [Zone 0 1, Zone 1 2, Zone 2 3])

    , test "Overlaying right across some gappy zones should add several" <|
      assertEqual
      [Add 0 (Zone -10 0), Add 2 (Zone 1 4), Add 4 (Zone 5 10)]
      (overlay (Zone -10 10) [Zone 0 1, Zone 4 5])

    , test "Overlaying and ending in the middle of a zone should split that last zone" <|
      assertEqual
      [Add 2 (Zone 2 4), Subst 3 [Zone 4 4.5, Zone 4.5 5]]
      (overlay (Zone 0 4.5) [Zone 0 1, Zone 1 2, Zone 4 5])

    , test "Overlaying from a gap to beyond the end should add several" <|
      assertEqual
      [Add 1 (Zone 2 4), Add 3 (Zone 5 7)]
      (overlay (Zone 2 7) [Zone 0 1, Zone 4 5])

    , test "Overlaying across continuous zones should split at the start and end only" <|
      assertEqual
      [Add 0 (Zone -1 0), Add 4 (Zone 5 6)]
      (overlay (Zone -1 6) [Zone 0 1, Zone 1 4, Zone 4 5])

    , test "Overlaying on continuous zones and which matches zone start and end should change nothing" <|
      assertEqual
      []
      (overlay (Zone 1 9) [Zone 0 1, Zone 1 4, Zone 4 5, Zone 5 9, Zone 9 10])

    , test "Overlaying in the middle of a zone should split it into three" <|
      assertEqual
      [Subst 1 [Zone 1 2, Zone 2 4], Subst 2 [Zone 2 3, Zone 3 4]]
      (overlay (Zone 2 3) [Zone 0 1, Zone 1 4, Zone 4 5])

    , test "Overlaying onto nothing should add just that one zone" <|
      assertEqual
      [Add 0 (Zone 2 3)]
      (overlay (Zone 2 3) [])

    ]

applyTest : Test
applyTest =
    suite "applyTest"

    [ test "Applying Add at the start should work" <|
      assertEqual
      [Zone -10 0, Zone 0 10, Zone 10 20]
      (apply (Add 0 (Zone -10 0)) [Zone 0 10, Zone 10 20])

    , test "Applying Add at the start of an empty list should work" <|
      assertEqual
      [Zone -10 0]
      (apply (Add 0 (Zone -10 0)) [])

    , test "Applying Add in the middle should work" <|
      assertEqual
      [Zone 0 10, Zone 10 15, Zone 15 20, Zone 20 30]
      (apply (Add 1 (Zone 10 15)) [Zone 0 10, Zone 15 20, Zone 20 30])

    , test "Applying Add at the end should work" <|
      assertEqual
      [Zone 0 10, Zone 10 20, Zone 20 30, Zone 30 40]
      (apply (Add 3 (Zone 30 40)) [Zone 0 10, Zone 10 20, Zone 20 30])

    , test "Applying Add at a negative point should have no effect" <|
      assertEqual
      [Zone 0 10, Zone 10 20, Zone 20 30]
      (apply (Add -1 (Zone 30 40)) [Zone 0 10, Zone 10 20, Zone 20 30])

    , test "Applying Add beyond the end should have no effect" <|
      assertEqual
      [Zone 0 10, Zone 10 20, Zone 20 30]
      (apply (Add 4 (Zone 30 40)) [Zone 0 10, Zone 10 20, Zone 20 30])

    , test "Applying Subst at the start should work" <|
      assertEqual
      [Zone 0 5, Zone 5 10, Zone 10 20]
      (apply (Subst 0 [Zone 0 5, Zone 5 10]) [Zone 0 10, Zone 10 20])

    , test "Applying Subst at index 0 of an empty list should have no effect" <|
      assertEqual
      []
      (apply (Subst 0 [Zone 0 5, Zone 5 10]) [])

    , test "Applying Subst in the middle should work" <|
      assertEqual
      [Zone 0 10, Zone 10 15, Zone 15 20, Zone 20 30]
      (apply (Subst 1 [Zone 10 15, Zone 15 20]) [Zone 0 10, Zone 10 20, Zone 20 30])

    , test "Applying Subst at the end should work" <|
      assertEqual
      [Zone 0 10, Zone 10 20, Zone 20 25, Zone 25 30]
      (apply (Subst 2 [Zone 20 25, Zone 25 30]) [Zone 0 10, Zone 10 20, Zone 20 30])

    , test "Applying Subst at a negative point should have no effect" <|
      assertEqual
      [Zone 0 10, Zone 10 20, Zone 20 30]
      (apply (Subst -1 [Zone -10 -5, Zone -5 0]) [Zone 0 10, Zone 10 20, Zone 20 30])

    , test "Applying Subst beyond the end should have no effect" <|
      assertEqual
      [Zone 0 10, Zone 10 20, Zone 20 30]
      (apply (Subst 3 [Zone 30 35, Zone 35 40]) [Zone 0 10, Zone 10 20, Zone 20 30])

    , test "Applying NoChange should have no effect" <|
      assertEqual
      [Zone 0 10, Zone 10 20, Zone 20 30]
      (apply NoChange [Zone 0 10, Zone 10 20, Zone 20 30])

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

    , test "Adding segment that is outside zones should add it" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 30 40)
          model = Model [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, zone3, Zone 30 40]
          (addSegment seg model |> .zones)

    , test "Adding segment that spans gaps and splits a zone should add multiple zones" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 30 40
          seg = Segment 40 (Zone -2 31)
          model = Model [] [zone1, zone2, zone3] []
      in
          assertEqual
          [Zone -2 0, zone1, zone2, Zone 20 30, Zone 30 31, Zone 31 40]
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

    , test "Adding segment that adds and splits vars should be okay" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 30 40
          -- This Segment add two vars and split the last one
          seg = Segment 65 (Zone -2 31)
          con1 = Constraint [1, 1, 1] 100
          con2 = Constraint [1, 1, 0] 40
          con3 = Constraint [0, 1, 1] 50
          model = Model [] [zone1, zone2, zone3] [con1, con2, con3]
      in
          assertEqual
          [ Constraint [0, 1, 1, 0, 1, 1] 100
          , Constraint [0, 1, 1, 0, 0, 0] 40
          , Constraint [0, 0, 1, 0, 1, 1] 50
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

applyToCoeffsTest : Test
applyToCoeffsTest =
    suite "applyToCoeffsTest"

    [ test "Applying Add at the start should work" <|
      assertEqual
      [0, 1, 1]
      (applyToCoeffs (Add 0 (Zone -10 0)) [1, 1])

    , test "Applying Add at the start of an empty list should work" <|
      assertEqual
      [0]
      (applyToCoeffs (Add 0 (Zone -10 0)) [])

    , test "Applying Add in the middle should work" <|
      assertEqual
      [1, 0, 1, 1]
      (applyToCoeffs (Add 1 (Zone 10 15)) [1, 1, 1])

    , test "Applying Add at the end should work" <|
      assertEqual
      [1, 1, 1, 0]
      (applyToCoeffs (Add 3 (Zone 30 40)) [1, 1, 1])

    , test "Applying Add at a negative point should have no effect" <|
      assertEqual
      [1, 0, 1]
      (applyToCoeffs (Add -1 (Zone 30 40)) [1, 0, 1])

    , test "Applying Add beyond the end should have no effect" <|
      assertEqual
      [1, 0, 1]
      (applyToCoeffs (Add 4 (Zone 30 40)) [1, 0, 1])

      , test "Applying Subst two at the start for coeff 0 should work" <|
      assertEqual
      [0, 0, 1]
      (applyToCoeffs (Subst 0 [Zone 0 5, Zone 5 10]) [0, 1])

      , test "Applying Subst two at the start for coeff 1 should work" <|
      assertEqual
      [1, 1, 0]
      (applyToCoeffs (Subst 0 [Zone 0 5, Zone 5 10]) [1, 0])

      , test "Applying Subst three at the start for coeff 1 should work" <|
      assertEqual
      [1, 1, 1, 0]
      (applyToCoeffs (Subst 0 [Zone 0 5, Zone 5 10, Zone 10 15]) [1, 0])

    , test "Applying Subst at index 0 of an empty list should have no effect" <|
      assertEqual
      []
      (applyToCoeffs (Subst 0 [Zone 0 5, Zone 5 10]) [])

    , test "Applying Subst two in the middle for coeff 0 should work" <|
      assertEqual
      [1, 0, 0, 1]
      (applyToCoeffs (Subst 1 [Zone 10 15, Zone 15 20]) [1, 0, 1])

    , test "Applying Subst two in the middle for coeff 1 should work" <|
      assertEqual
      [0, 1, 1, 0]
      (applyToCoeffs (Subst 1 [Zone 10 15, Zone 15 20]) [0, 1, 0])

    , test "Applying Subst three in the middle for coeff 0 should work" <|
      assertEqual
      [1, 0, 0, 0, 1]
      (applyToCoeffs (Subst 1 [Zone 10 15, Zone 15 20, Zone 20 25]) [1, 0, 1])

    , test "Applying Subst three in the middle for coeff 1 should work" <|
      assertEqual
      [0, 1, 1, 1, 0]
      (applyToCoeffs (Subst 1 [Zone 10 15, Zone 15 20, Zone 20 25]) [0, 1, 0])

    , test "Applying Subst at the end should work" <|
      assertEqual
      [1, 0, 1, 1]
      (applyToCoeffs (Subst 2 [Zone 20 25, Zone 25 30]) [1, 0, 1])

    , test "Applying Subst at a negative point should have no effect" <|
      assertEqual
      [1, 1, 0]
      (applyToCoeffs (Subst -1 [Zone -10 -5, Zone -5 0]) [1, 1, 0])

    , test "Applying Subst beyond the end should have no effect" <|
      assertEqual
      [0, 1, 1]
      (applyToCoeffs (Subst 3 [Zone 30 35, Zone 35 40]) [0, 1, 1])

    , test "Applying NoChange should have no effect" <|
      assertEqual
      [0, 1, 1]
      (applyToCoeffs NoChange [0, 1, 1])

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

addConstraintTest : Test
addConstraintTest =
    suite "addConstraintTest"

    [ test "Constraint should be added at end" <|
      let
          seg1 = Segment 100 (Zone 0 30)
          seg2 = Segment 40 (Zone 0 20)
          seg3 = Segment 50 (Zone 20 30)
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          con1 = Constraint [1, 1, 1] 100
          con2 = Constraint [1, 1, 0] 40
          con3 = Constraint [0, 1, 1] 50
          model = Model [seg1, seg2, seg3] [zone1, zone2, zone3] [con1, con2]
      in
          assertEqual
          [con1, con2, con3]
          (addConstraint con3 model |> .constraints)
    ]

isSubcoeffTest : Test
isSubcoeffTest =
    suite "isSubcoeffTest"

    [ test "Smaller list is not subcoeff of larger list" <|
      assertEqual
      False
      (isSubcoeff [1, 1] [1, 1, 1])

    , test "Larger list is not subcoeff of smaller list" <|
      assertEqual
      False
      (isSubcoeff [1, 1, 1] [1, 1])

    , test "List is subcoeff of itself" <|
      assertEqual
      True
      (isSubcoeff [1, 1, 0] [1, 1, 0])

    , test "All zeros is subcoeff of anything with non-zeros (and same length)" <|
      assertEqual
      True
      (isSubcoeff [0, 0, 0] [1, 1, 0])

    , test "Something with non-zeros is subcoeff of that plus other non-zeros" <|
      assertEqual
      True
      (isSubcoeff [1, 0, 0, 2, 0] [1, 0, 1, 2, 0])

    ]

subtractTest : Test
subtractTest =
    suite "subtractTest"

    [ test "Basic subtraction should work" <|
      assertEqual
      (Constraint [1, 3, -3] 12)
      (subtract (Constraint [2, 3, 4] 90) (Constraint [1, 0, 7] 78))

    , test "Longer subtract shorter should ignore extra coefficients" <|
      assertEqual
      (Constraint [-2, 2] 4)
      (subtract (Constraint [2, 3, 4, 1] 10) (Constraint [4, 1] 6))

    , test "Shorter subtract longer should ignore extra coefficients" <|
      assertEqual
      (Constraint [2, -2] 3)
      (subtract (Constraint [4, 1] 20) (Constraint [2, 3, 4, 5] 17))

    ]

deriveOnceTest : Test
deriveOnceTest =
    suite "deriveOnceTest"

    [ test "Simple derivation should work" <|
      let
        con1 = Constraint [1, 1, 1] 100
        con2 = Constraint [0, 1, 1] 55
        seed = Constraint [0, 0, 1] 30
        -- This subtracting seed from the others should give these...
        res1 = Constraint [1, 1, 0] 70
        res2 = Constraint [0, 1, 0] 25
      in
        assertEqual
        [res1, res2]
        (deriveOnce [con1, con2] seed)

    , test "deriveOnce should work when the seed is the larger constraint, to be subtracted from" <|
      let
        con1 = Constraint [0, 1, 1] 65
        con2 = Constraint [0, 1, 0] 20
        seed = Constraint [1, 1, 1] 80
        -- Subtracting others from seed should give these...
        res1 = Constraint [1, 0, 0] 15
        res2 = Constraint [1, 0, 1] 60
      in
        assertEqual
        [res1, res2]
        (deriveOnce [con1, con2] seed)

    , test "deriveOnce should skip constraint where it can't subtract" <|
      let
        con1 = Constraint [0, 1, 1] 75
        con2 = Constraint [1, 1, 1] 90
        seed = Constraint [1, 1, 0] 30
        -- Subtracting where we can should give this...
        res1 = Constraint [0, 0, 1] 60
      in
        assertEqual
        [res1]
        (deriveOnce [con1, con2] seed)

    , test "deriveOnce returns nothing if we're starting with no constraints" <|
      let
        seed = Constraint [1, 1, 0] 30
      in
        assertEqual
        []
        (deriveOnce [] seed)

    ]

deriveAllTest : Test
deriveAllTest =
    suite "deriveAllTest"

    [ test "Simple derivation of all should work" <|
      let
        con1 = Constraint [1, 1, 1] 100
        con2 = Constraint [0, 1, 1] 55
        seed = Constraint [0, 0, 1] 30
        -- This subtracting seed from the others should give these...
        res1 = Constraint [1, 1, 0] 70
        res2 = Constraint [0, 1, 0] 25
        -- And res2 should give this...
        res3 = Constraint [1, 0, 1] 75
        -- ...which will in turn give this...
        res4 = Constraint [1, 0, 0] 45
      in
        assertEqual
        [con1, con2, seed, res1, res2, res3, res4]
        (deriveAll [con1, con2] seed)

    , test "deriveAll should work when the seed is the larger constraint, to be subtracted from" <|
      let
        con1 = Constraint [0, 1, 1] 65
        con2 = Constraint [0, 1, 0] 20
        seed = Constraint [1, 1, 1] 80
        -- This subtracting others from seed should give these...
        res1 = Constraint [1, 0, 0] 15
        res2 = Constraint [1, 0, 1] 60
        -- Then subtracting these two gives...
        res3 = Constraint [0, 0, 1] 45
        -- And subtracting that from the seed gives
        res4 = Constraint [1, 1, 0] 35
      in
        assertEqual
        [con1, con2, seed, res1, res2, res3, res4]
        (deriveAll [con1, con2] seed)

    , test "deriveAll should skip constraint where it can't subtract" <|
      let
        con1 = Constraint [0, 1, 1] 75
        con2 = Constraint [1, 1, 1] 90
        seed = Constraint [1, 1, 0] 30
        -- Subtracting where we can should give this...
        res1 = Constraint [0, 0, 1] 60
        -- And that should in turn give this...
        res2 = Constraint [0, 1, 0] 15
        -- And from that we can derive these...
        res3 = Constraint [1, 0, 1] 75
        res4 = Constraint [1, 0, 0] 15
      in
        assertEqual
        [con1, con2, seed, res1, res2, res3, res4]
        (deriveAll [con1, con2] seed)

    , test "deriveAll returns just the seed if we're starting with no constraints" <|
      let
        seed = Constraint [1, 1, 0] 30
      in
        assertEqual
        [seed]
        (deriveAll [] seed)

    ]

modelTest : Test
modelTest =
    suite "modelTest"

    [ test "Basic build of a model should work" <|
      let
        seg1 = Segment 50 (Zone 5 15)
        seg2 = Segment 60 (Zone 0 inf)
        seg3 = Segment 5 (Zone 15 inf)
        expected =
            { segments =
                [ seg1, seg2, seg3 ]
            , zones =
                [ Zone -inf 0, Zone 0 5, Zone 5 15, Zone 15 inf ]
            , constraints =
                [ Constraint [1, 1, 1, 1] 100 -- Baseline, always
                , Constraint [0, 0, 1, 0] 50  -- seg1
                , Constraint [1, 1, 0, 1] 50  --   Baseline - seg1  [A]
                , Constraint [0, 1, 1, 1] 60  -- seg2
                , Constraint [1, 0, 0, 0] 40  --   Baseline - seg2  [B]
                , Constraint [0, 1, 0, 1] 10  --   seg2 - seg1      [C]
                , Constraint [1, 0, 1, 0] 90  --   Baseline - [C]   [D]
                , Constraint [0, 0, 0, 1]  5  -- seg3
                , Constraint [1, 1, 1, 0] 95  --   Baseline - seg3  [E]
                , Constraint [1, 1, 0, 0] 45  --   [E] - seg1       [F]
                , Constraint [0, 1, 1, 0] 55  --   [E] - [B]        [G]
                , Constraint [0, 1, 0, 0]  5  --   [E] - [D]        [H]
                , Constraint [0, 0, 1, 1] 55  --   Baseline - [F]   [I]
                , Constraint [1, 0, 0, 1] 45  --   Baseline - [G]   [J]
                , Constraint [1, 0, 1, 1] 95  --   Baseline - [H]
                ]
            }
      in
        assertEqual
        expected
        (model [seg1, seg2, seg3])

    ]
