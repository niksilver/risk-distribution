module ZoneTest exposing (all)

import Zone exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "ZoneTest"
    [ infinityTest
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
      (Zone -inf inf |> splitOne 10)

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
      ([Zone -inf inf] |> split 11)

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
