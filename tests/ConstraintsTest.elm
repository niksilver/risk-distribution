module ConstraintsTest exposing (all)

import Constraints exposing (..)
import Zone exposing
    ( Zone, inf
    , Change (Subst, Add, NoChange)
    )

import ElmTest exposing (..)

all : Test
all =
    suite "ConstraintsTest"
    [ constraintToStringTest
    , derivationToStringTest
    , addSegmentTestForNewSegment
    , addSegmentTestForNewZone
    , addSegmentTestForNewVariables
    , applyToCoeffsTest
    , constraintTest
    , isSubcoeffTest
    , subtractTest
    , deriveOnceTest
    , deriveAllTest
    , modelTest
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

derivationToStringTest : Test
derivationToStringTest =
    suite "derivationToStringTest"

    [ test "Should work for 'a + c = 33' and no sources" <|
      assertEqual
--     a + b + c = ..
      "a     + c = 33 ()"
      (Derivation (Constraint [1, 0, 1] 33) [] |> derivationToString)

    , test "Should work for 'a + b + c = 65' with one source" <|
      assertEqual
      "a + b + c = 65 (3)"
      (Derivation (Constraint [1, 1, 1] 65) [3] |> derivationToString)

    , test "Should work for 'a + b + c = 65' with two sources" <|
      assertEqual
      "a + b + c = 65 (4, 3)"
      (Derivation (Constraint [1, 1, 1] 65) [4, 3] |> derivationToString)

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

    , test "deriveOnce will not derive all-zero coefficients if given different constraints with same coeffs" <|
      let
        con1 = Constraint [0, 1, 1] 65
        seed = Constraint [0, 1, 1] 20
      in
        assertEqual
        []
        (deriveOnce [con1] seed)

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

    [ test "A model built from nothing should at least include the base segment" <|
      let
        expected =
            { segments =
                [ baseSegment ]
            , zones =
                [ Zone -inf inf ]
            , constraints =
                [ Constraint [1] 100
                ]
            }
      in
        assertEqual
        expected
        (model [])

    , test "Basic build of a model should work" <|
      let
        seg1 = Segment 50 (Zone 5 15)
        seg2 = Segment 60 (Zone 0 inf)
        seg3 = Segment 5 (Zone 15 inf)
        expected =
            { segments =
                [ baseSegment, seg1, seg2, seg3 ]
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
