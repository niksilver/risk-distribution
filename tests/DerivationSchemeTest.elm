module DerivationSchemeTest exposing (all)

import DerivationScheme exposing (..)

import Zone exposing (inf, Zone)
import Constraint as Cons exposing (Segment, Constraint)
import Derivation exposing (Derivation)

import ElmTest exposing (..)


all : Test
all =
    suite "DerivationSchemeTest"
    [ addSegmentTestForNewSegment -- Deprecate!
    , addForSegmentsTest
    , addSegmentTestForNewZone -- Deprecate!
    , addForZonesTest
    , addSegmentTestForNewVariables
    , schemeTest
    ]


-- A quick way of creating a Derivation

deriv : List Int -> Int -> List Int -> Derivation
deriv coeffs pc src =
    Derivation (Constraint coeffs pc) src


-- The tests...

-- Deprecate!
addSegmentTestForNewSegment : Test
addSegmentTestForNewSegment =
    suite "addSegmentTestForNewSegment"

    [ test "Adding first segment should add it okay" <|
      let
          segment = Segment 40 (Zone -inf 0)
          scheme = Scheme [] [] []
      in
          assertEqual
          [ segment ]
          (addSegment segment scheme |> .segments)

    , test "Adding second segment should add it to the end of segment list" <|
      let
          segNew = Segment 40 (Zone -inf 0)
          seg1 = Segment 50 (Zone -10 10)
          seg2 = Segment 100 (Zone -inf inf)
          scheme = Scheme [seg1, seg2] [] []
      in
          assertEqual
          [ seg1, seg2, segNew ]
          (addSegment segNew scheme |> .segments)

    ]

addForSegmentsTest : Test
addForSegmentsTest =
    suite "addForSegmentsTest"

    [ test "Adding first segment should add it okay" <|
      let
          segment = Segment 40 (Zone -inf 0)
          scheme = Scheme [] [] []
      in
          assertEqual
          [ segment ]
          (addForSegments [segment] scheme)

    , test "Adding third segment should add it to the end of segment list" <|
      let
          segNew = Segment 40 (Zone -inf 0)
          seg1 = Segment 50 (Zone -10 10)
          seg2 = Segment 100 (Zone -inf inf)
          scheme = Scheme [seg1, seg2] [] []
      in
          assertEqual
          [ seg1, seg2, segNew ]
          (addForSegments [segNew] scheme)

    , test "Adding no segments should change nothing" <|
      let
          seg1 = Segment 50 (Zone -10 10)
          seg2 = Segment 100 (Zone -inf inf)
          scheme = Scheme [seg1, seg2] [] []
      in
          assertEqual
          [ seg1, seg2 ]
          (addForSegments [] scheme)

    , test "Adding multiple segments should add them to the end of segment list" <|
      let
          segNew1 = Segment 40 (Zone -inf 0)
          segNew2 = Segment 100 (Zone -inf inf)
          seg1 = Segment 50 (Zone -10 10)
          scheme = Scheme [seg1] [] []
      in
          assertEqual
          [ seg1, segNew1, segNew2 ]
          (addForSegments [segNew1, segNew2] scheme)

    ]

-- Deprecate!
addSegmentTestForNewZone : Test
addSegmentTestForNewZone =
    suite "addSegmentTestForNewZone"

    [ test "Adding segment that splits one zone with its 'from' should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 25 30)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, Zone 20 25, Zone 25 30]
          (addSegment seg scheme |> .zones)

    , test "Adding segment that splits one zone with its 'to' should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 10 15)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, Zone 10 15, Zone 15 20, zone3]
          (addSegment seg scheme |> .zones)

    , test "Adding segment that splits one zone into three should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 5 6)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [Zone 0 5, Zone 5 6, Zone 6 10, zone2, zone3]
          (addSegment seg scheme |> .zones)

    , test "Adding segment that splits two neighbouring zones should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 50 (Zone 15 25)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, Zone 10 15, Zone 15 20, Zone 20 25, Zone 25 30]
          (addSegment seg scheme |> .zones)

    , test "Adding segment that splits two distant zones should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 65 (Zone 9 21)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [Zone 0 9, Zone 9 10, zone2, Zone 20 21, Zone 21 30]
          (addSegment seg scheme |> .zones)

    , test "Adding segment that is outside zones should add it" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 30 40)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, zone3, Zone 30 40]
          (addSegment seg scheme |> .zones)

    , test "Adding segment that spans gaps and splits a zone should add multiple zones" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 30 40
          seg = Segment 40 (Zone -2 31)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [Zone -2 0, zone1, zone2, Zone 20 30, Zone 30 31, Zone 31 40]
          (addSegment seg scheme |> .zones)

    , test "Adding segment that splits no zones should not change zones" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 10 30)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, zone3]
          (addSegment seg scheme |> .zones)

    ]

addForZonesTest : Test
addForZonesTest =
    suite "addForZonesTest"

    [ test "Adding one segment that splits one zone with its 'from' should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 25 30)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, Zone 20 25, Zone 25 30]
          (addForZones [seg] scheme)

    , test "Adding one segment that splits one zone with its 'to' should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 10 15)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, Zone 10 15, Zone 15 20, zone3]
          (addForZones [seg] scheme)

    , test "Adding one segment that splits one zone into three should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 5 6)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [Zone 0 5, Zone 5 6, Zone 6 10, zone2, zone3]
          (addForZones [seg] scheme)

    , test "Adding one segment that splits two neighbouring zones should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 50 (Zone 15 25)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, Zone 10 15, Zone 15 20, Zone 20 25, Zone 25 30]
          (addForZones [seg] scheme)

    , test "Adding one segment that splits two distant zones should work" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 65 (Zone 9 21)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [Zone 0 9, Zone 9 10, zone2, Zone 20 21, Zone 21 30]
          (addForZones [seg] scheme)

    , test "Adding one segment that is outside zones should add it" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 30 40)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, zone3, Zone 30 40]
          (addForZones [seg] scheme)

    , test "Adding one segment that spans gaps and splits a zone should add multiple zones" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 30 40
          seg = Segment 40 (Zone -2 31)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [Zone -2 0, zone1, zone2, Zone 20 30, Zone 30 31, Zone 31 40]
          (addForZones [seg] scheme)

    , test "Adding one segment that splits no zones should not change zones" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg = Segment 40 (Zone 10 30)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, zone3]
          (addForZones [seg] scheme)

    , test "Adding no segments should yield no change in zones" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [zone1, zone2, zone3]
          (addForZones [] scheme)

    , test "Adding two segments should yield appropriate change in zones" <|
      let
          zone1 = Zone 0 10
          zone2 = Zone 10 20
          zone3 = Zone 20 30
          seg1 = Segment 40 (Zone 15 17)
          seg2 = Segment 40 (Zone 20 25)
          scheme = Scheme [] [zone1, zone2, zone3] []
      in
          assertEqual
          [ Zone 0 10
          , Zone 10 15
          , Zone 15 17
          , Zone 17 20
          , Zone 20 25
          , Zone 25 30
          ]
          (addForZones [seg1, seg2] scheme)

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
          scheme = Scheme [] [zone1, zone2, zone3] [der1, der2, der3]
      in
          assertEqual
          [ deriv [1, 1, 1, 1] 100 [0]
          , deriv [1, 1, 0, 0] 40  [1]
          , deriv [0, 1, 1, 1] 50  [2]
          ]
          (addSegment seg scheme |> .derivations)

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
          scheme = Scheme [] [zone1, zone2, zone3] [der1, der2, der3]
      in
          assertEqual
          [ deriv [1, 1, 1, 1, 1] 100 [0]
          , deriv [1, 1, 1, 0, 0] 40  [1]
          , deriv [0, 0, 1, 1, 1] 50  [2]
          ]
          (addSegment seg scheme |> .derivations)

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
          scheme = Scheme [] [zone1, zone2, zone3] [der1, der2, der3]
      in
          assertEqual
          [ deriv [0, 1, 1, 0, 1, 1] 100 [0]
          , deriv [0, 1, 1, 0, 0, 0] 40  [1]
          , deriv [0, 0, 1, 0, 1, 1] 50  [2]
          ]
          (addSegment seg scheme |> .derivations)

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
          scheme = Scheme [] [zone1, zone2, zone3] [der1, der2, der3]
      in
          assertEqual
          [der1, der2, der3]
          (addSegment seg scheme |> .derivations)

    ]

schemeTest : Test
schemeTest =
    suite "schemeTest"

    [ test "A scheme built from nothing should at least include the base segment" <|
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
        (scheme [])

    , test "Basic build of a scheme should work" <|
      let
        seg1 = Segment 50 (Zone 5 15)
        seg2 = Segment 60 (Zone 0 inf)
        seg3 = Segment 5 (Zone 15 inf)
        expected =
            { segments =
                [ Cons.baseSegment, seg1, seg2, seg3 ]
            , zones =
                [ Zone -inf 0, Zone 0 5, Zone 5 15, Zone 15 inf ]
            -- Some of the derivations will be reversed, so we'll use a concat
            -- and the odd reverse to lay them out here in proper forward order
            , derivations =
                [ [ deriv [1, 1, 1, 1] 100 [0]            -- Baseline, always
                  , deriv [0, 0, 1, 0] 50  [1]            -- seg1
                  , deriv [1, 1, 0, 1] 50  [0, 1]         --   Baseline - seg1  [A]
                  , deriv [0, 1, 1, 1] 60  [2]          ] -- seg2
                , [ deriv [1, 0, 0, 0] 40  [0, 2]         --   Baseline - seg2  [B]
                  , deriv [0, 1, 0, 1] 10  [2, 1]       ] --   seg2 - seg1      [C]
                  |> List.reverse
                , [ deriv [1, 0, 1, 0] 90  [0, 2, 1]      --   Baseline - [C]   [D]
                  , deriv [0, 0, 0, 1]  5  [3]          ] -- seg3
                , [ deriv [1, 1, 1, 0] 95  [0, 3]         --   Baseline - seg3  [E]
                  , deriv [1, 1, 0, 0] 45  [0, 1, 3]      --   [A] - seg3       [F]
                  , deriv [0, 1, 1, 0] 55  [2, 3]         --   seg2 - seg3      [G]
                  , deriv [0, 1, 0, 0]  5  [2, 1, 3]    ] --   [C] - seg3       [H]
                  |> List.reverse
                , [ deriv [0, 0, 1, 1] 55  [2, 2, 1, 3]   --   seg2 - [H]       [I]
                  , deriv [1, 0, 0, 1] 45  [0, 1, 2, 1, 3] --  [A] - [H]        [J]
                  , deriv [1, 0, 1, 1] 95  [0, 2, 1, 3] ] --   Baseline - [H]
                ]
                |> List.concat
            }
      in
        assertEqual
        expected
        (scheme [seg1, seg2, seg3])

    ]
