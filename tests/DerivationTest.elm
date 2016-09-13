module DerivationTest exposing (all)

import Derivation exposing (..)

import Zone exposing (inf, Zone)
import Constraint as Cons exposing (Segment, Constraint)

import ElmTest exposing (..)


all : Test
all =
    suite "DerivationTest"
    [ derivationToStringTest
    , subtractTest
    , deriveOnceTest
    , deriveAllTest
    , addSegmentTestForNewSegment
    , addSegmentTestForNewZone
    , addSegmentTestForNewVariables
    , modelTest
    ]

-- A quick way of creating a Derivation

deriv : List Int -> Int -> List Int -> Derivation
deriv coeffs pc src =
    Derivation (Constraint coeffs pc) src

-- The tests...

derivationToStringTest : Test
derivationToStringTest =
    suite "derivationToStringTest"

    [ test "Should work for 'a + c = 33' and no sources" <|
      assertEqual
--     a + b + c = ..
      "a     + c = 33 ()"
      (deriv [1, 0, 1] 33 [] |> derivationToString)

    , test "Should work for 'a + b + c = 65' with one source" <|
      assertEqual
      "a + b + c = 65 (3)"
      (deriv [1, 1, 1] 65 [3] |> derivationToString)

    , test "Should work for 'a + b + c = 65' with two sources" <|
      assertEqual
      "a + b + c = 65 (4, 3)"
      (deriv [1, 1, 1] 65 [4, 3] |> derivationToString)

    ]

subtractTest : Test
subtractTest =
    suite "subtractTest"

    [ test "Basic subtraction should work" <|
      assertEqual
      (deriv [1, 3, -3] 12 [2, 1])
      (subtract
        (deriv [2, 3, 4] 90 [2])
        (deriv [1, 0, 7] 78 [1])
      )

    , test "Longer subtract shorter should ignore extra coefficients" <|
      assertEqual
      (deriv [-2, 2] 4 [3, 0, 1])
      (subtract
        (deriv [2, 3, 4, 1] 10 [3])
        (deriv [4, 1]        6 [0, 1])
      )

    , test "Shorter subtract longer should ignore extra coefficients" <|
      assertEqual
      (deriv [2, -2] 3 [4])
      (subtract
        (deriv [4, 1]       20 [])
        (deriv [2, 3, 4, 5] 17 [4])
      )

    ]

deriveOnceTest : Test
deriveOnceTest =
    suite "deriveOnceTest"

    [ test "Simple derivation should work" <|
      let
        der1 = deriv [1, 1, 1] 100 [0]
        der2 = deriv [0, 1, 1] 55  [1]
        seed = deriv [0, 0, 1] 30  [2]
        -- This subtracting seed from the others should give these...
        res1 = deriv [1, 1, 0] 70  [0, 2]
        res2 = deriv [0, 1, 0] 25  [1, 2]
      in
        assertEqual
        [res1, res2]
        (deriveOnce [der1, der2] seed)

    , test "deriveOnce should work when the seed is the larger constraint, to be subtracted from" <|
      let
        der1 = deriv [0, 1, 1] 65  [0]
        der2 = deriv [0, 1, 0] 20  [1]
        seed = deriv [1, 1, 1] 80  [2]
        -- Subtracting others from seed should give these...
        res1 = deriv [1, 0, 0] 15  [2, 0]
        res2 = deriv [1, 0, 1] 60  [2, 1]
      in
        assertEqual
        [res1, res2]
        (deriveOnce [der1, der2] seed)

    , test "deriveOnce should skip constraint where it can't subtract" <|
      let
        der1 = deriv [0, 1, 1] 75  [0]
        der2 = deriv [1, 1, 1] 90  [1]
        seed = deriv [1, 1, 0] 30  [2]
        -- Subtracting where we can should give this...
        res1 = deriv [0, 0, 1] 60  [1, 2]
      in
        assertEqual
        [res1]
        (deriveOnce [der1, der2] seed)

    , test "deriveOnce returns nothing if we're starting with no constraints" <|
      let
        seed = deriv [1, 1, 0] 30  [0]
      in
        assertEqual
        []
        (deriveOnce [] seed)

    , test "deriveOnce will not derive all-zero coefficients if given different constraints with same coeffs" <|
      let
        der1 = deriv [0, 1, 1] 65  [0]
        seed = deriv [0, 1, 1] 20  [1]
      in
        assertEqual
        []
        (deriveOnce [der1] seed)

    ]

deriveAllTest : Test
deriveAllTest =
    suite "deriveAllTest"

    [ test "Simple derivation of all should work" <|
      let
        der1 = deriv [1, 1, 1] 100 [0]
        der2 = deriv [0, 1, 1] 55  [1]
        seed = deriv [0, 0, 1] 30  [2]
        -- This subtracting seed from the others should give these...
        res1 = deriv [1, 1, 0] 70  [0, 2]
        res2 = deriv [0, 1, 0] 25  [1, 2]
        -- And res2 should give this...
        res3 = deriv [1, 0, 1] 75  [0, 1, 2]
        -- And res1 and res2 should give this
        res4 = deriv [1, 0, 0] 45  [0, 2, 1, 2]
      in
        assertEqual
        [der1, der2, seed, res1, res2, res3, res4]
        (deriveAll [der1, der2] seed)

    , test "deriveAll should work when the seed is the larger constraint, to be subtracted from" <|
      let
        der1 = deriv [0, 1, 1] 65  [0]
        der2 = deriv [0, 1, 0] 20  [1]
        seed = deriv [1, 1, 1] 80  [2]
        -- This subtracting others from seed should give these...
        res1 = deriv [1, 0, 0] 15  [2, 0]
        res2 = deriv [1, 0, 1] 60  [2, 1]
        -- Then subtracting these two gives...
        res3 = deriv [0, 0, 1] 45  [2, 1, 2, 0]
        -- And subtracting that from the seed gives
        res4 = deriv [1, 1, 0] 35  [2, 2, 1, 2, 0]
      in
        assertEqual
        [der1, der2, seed, res1, res2, res3, res4]
        (deriveAll [der1, der2] seed)

    , test "deriveAll should skip constraint where it can't subtract" <|
      let
        der1 = deriv [0, 1, 1] 75  [0]
        der2 = deriv [1, 1, 1] 90  [1]
        seed = deriv [1, 1, 0] 30  [2]
        -- Subtracting where we can should give this...
        res1 = deriv [0, 0, 1] 60  [1, 2]
        -- And that should in turn give this...
        res2 = deriv [0, 1, 0] 15  [0, 1, 2]
        -- And from that we can derive these...
        res3 = deriv [1, 0, 1] 75  [1, 0, 1, 2]
        res4 = deriv [1, 0, 0] 15  [2, 0, 1, 2]
      in
        assertEqual
        [der1, der2, seed, res1, res2, res3, res4]
        (deriveAll [der1, der2] seed)

    , test "deriveAll returns just the seed if we're starting with no constraints" <|
      let
        seed = deriv [1, 1, 0] 30  [0]
      in
        assertEqual
        [seed]
        (deriveAll [] seed)

    , test "deriveAll should terminate if there's an inconsistent 'between' derivation" <|
      -- This error derives from entering the following constraints:
      -- There's a 100% chance it's between 0 and 10  [1]
      -- There's a  50% chance it's between 2 and 8   [2]
      -- There's a  40% chance it's between 2 and 8   [3]
      -- (Plus the implicit 100% chance it's between -inf and inf)
      -- So the zones are: -inf to 0,  0 to 2,  2 to 8,  8 to 10,  10 to inf
      let
        der0 = deriv [1, 1, 1, 1, 1] 100  [0]
        der1 = deriv [0, 1, 1, 1, 0] 100  [1]
        der2 = deriv [0, 0, 1, 0, 0]  50  [2]
        der3 = deriv [0, 0, 1, 0, 0]  40  [3]
        res1 = deriveAll [der0] der1
        res2 = deriveAll res1 der2
      in
        assertEqual
        False
        (deriveAll res2 der3 |> List.isEmpty)

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
          der1 = deriv [1, 1, 1] 100 [0]
          der2 = deriv [1, 1, 0] 40  [1]
          der3 = deriv [0, 1, 1] 50  [2]
          model = Model [] [zone1, zone2, zone3] [der1, der2, der3]
      in
          assertEqual
          [ deriv [1, 1, 1, 1] 100 [0]
          , deriv [1, 1, 0, 0] 40  [1]
          , deriv [0, 1, 1, 1] 50  [2]
          ]
          (addSegment seg model |> .derivations)

    , test "Adding segment that substitutes two vars should be okay" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          -- This Segment should split first and third var
          seg = Segment 65 (Zone 9 21)
          der1 = deriv [1, 1, 1] 100 [0]
          der2 = deriv [1, 1, 0] 40  [1]
          der3 = deriv [0, 1, 1] 50  [2]
          model = Model [] [zone1, zone2, zone3] [der1, der2, der3]
      in
          assertEqual
          [ deriv [1, 1, 1, 1, 1] 100 [0]
          , deriv [1, 1, 1, 0, 0] 40  [1]
          , deriv [0, 0, 1, 1, 1] 50  [2]
          ]
          (addSegment seg model |> .derivations)

    , test "Adding segment that adds and splits vars should be okay" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 30 40
          -- This Segment add two vars and split the last one
          seg = Segment 65 (Zone -2 31)
          der1 = deriv [1, 1, 1] 100 [0]
          der2 = deriv [1, 1, 0] 40  [1]
          der3 = deriv [0, 1, 1] 50  [2]
          model = Model [] [zone1, zone2, zone3] [der1, der2, der3]
      in
          assertEqual
          [ deriv [0, 1, 1, 0, 1, 1] 100 [0]
          , deriv [0, 1, 1, 0, 0, 0] 40  [1]
          , deriv [0, 0, 1, 0, 1, 1] 50  [2]
          ]
          (addSegment seg model |> .derivations)

    , test "Adding segment that substitutes no vars should be okay" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          -- This Segment should split no vars
          seg = Segment 75 (Zone 10 30)
          der1 = deriv [1, 1, 1] 100 [0]
          der2 = deriv [1, 1, 0] 40  [1]
          der3 = deriv [0, 1, 1] 50  [2]
          model = Model [] [zone1, zone2, zone3] [der1, der2, der3]
      in
          assertEqual
          [der1, der2, der3]
          (addSegment seg model |> .derivations)

    ]

modelTest : Test
modelTest =
    suite "modelTest"

    [ test "A model built from nothing should at least include the base segment" <|
      let
        expected =
            { segments =
                [ Cons.baseSegment ]
            , zones =
                [ Zone -inf inf ]
            , derivations =
                [ deriv [1] 100  [0]
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
                [ Cons.baseSegment, seg1, seg2, seg3 ]
            , zones =
                [ Zone -inf 0, Zone 0 5, Zone 5 15, Zone 15 inf ]
            , derivations =
                [ deriv [1, 1, 1, 1] 100 [0]          -- Baseline, always
                , deriv [0, 0, 1, 0] 50  [1]          -- seg1
                , deriv [1, 1, 0, 1] 50  [0, 1]       --   Baseline - seg1  [A]
                , deriv [0, 1, 1, 1] 60  [2]          -- seg2
                , deriv [1, 0, 0, 0] 40  [0, 2]       --   Baseline - seg2  [B]
                , deriv [0, 1, 0, 1] 10  [2, 1]       --   seg2 - seg1      [C]
                , deriv [1, 0, 1, 0] 90  [0, 2, 1]    --   Baseline - [C]   [D]
                , deriv [0, 0, 0, 1]  5  [3]          -- seg3
                , deriv [1, 1, 1, 0] 95  [0, 3]       --   Baseline - seg3  [E]
                , deriv [1, 1, 0, 0] 45  [0, 1, 3]    --   [A] - seg3       [F]
                , deriv [0, 1, 1, 0] 55  [2, 3]       --   seg2 - seg3      [G]
                , deriv [0, 1, 0, 0]  5  [2, 1, 3]    --   [C] - seg3       [H]
                , deriv [0, 0, 1, 1] 55  [0, 0, 1, 3] --   Baseline - [F]   [I]
                , deriv [1, 0, 0, 1] 45  [0, 2, 3]    --   Baseline - [G]   [J]
                , deriv [1, 0, 1, 1] 95  [0, 2, 1, 3] --   Baseline - [H]
                ]
            }
      in
        assertEqual
        expected
        (model [seg1, seg2, seg3])

    ]
