module DerivationSchemeTest exposing (all)

import DerivationScheme exposing (..)

import Zone exposing (inf, Zone)
import Constraint as Cons exposing (Constraint)
import Segment exposing (Segment)
import Derivation exposing (Derivation)

import ElmTest exposing (..)


all : Test
all =
    suite "DerivationSchemeTest"
    [ derivationsTest
    , schemeTest
    ]


-- A quick way of creating a Derivation

deriv : List Int -> Int -> List Int -> Derivation
deriv coeffs pc src =
    Derivation (Constraint coeffs pc) src


-- The tests...

derivationsTest : Test
derivationsTest =
    suite "addForDerivationsTest"

    [ test "No segments or zones should yield no derivations" <|
      assertEqual
      []
      (derivations [] [])

    , test "One segment and zone should yield one derivation with correct source" <|
      let
          seg1 = Segment 75 (Zone 0 10)
          zone1 = Zone 0 10
          der1 = deriv [1] 75 [0]
      in
          assertEqual
          [der1]
          (derivations [seg1] [zone1])

    , test "Two segments and some zones should yield correct derivations with correct sources" <|
      let
          seg1 = Segment 75 (Zone 0 10)
          seg2 = Segment 50 (Zone 5 15)
          zone1 = Zone 0 5
          zone2 = Zone 5 10
          zone3 = Zone 10 15
          der1 = deriv [1, 1, 0] 75 [0]
          der2 = deriv [0, 1, 1] 50 [1]
      in
          assertEqual
          [der1, der2]
          (derivations [seg1, seg2] [zone1, zone2, zone3])

    ]

schemeTest : Test
schemeTest =
    suite "schemeTest"

    [ test "A scheme built from nothing should at least include the base segment" <|
      let
        expected =
            { segments =
                [ Segment.baseSegment ]
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
                [ Segment.baseSegment, seg1, seg2, seg3 ]
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
