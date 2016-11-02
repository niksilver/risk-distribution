module ErrorTest exposing (all)

import Error exposing (..)
import Zone exposing (Zone)
import Constraint exposing (Constraint)
import Segment exposing (Segment)
import Derivation exposing (Derivation)

import ElmTest exposing (..)

all : Test
all =
    suite "ErrorTest"
    [ findTestForNegative
    , findTestForContradiction
    ]

-- A quick way of creating a Derivation

deriv : List Int -> Int -> List Int -> Derivation
deriv coeffs pc src =
    Derivation (Constraint coeffs pc) src

-- The tests...

findTestForNegative : Test
findTestForNegative =
    suite "findTestForNegative"

    [ test "Zone of negative derivation should be found" <|
      let
        -- This doesn't make logical sense; it's just something with
        -- the appropriate error among the derivations.
        model =
            { segments =
                [ Segment 90 (Zone 0 10)
                , Segment 70 (Zone 5 15)
                ]
            , zones =
                [ Zone 0 5, Zone 5 10, Zone 10 15 ]
            , derivations =
                [ deriv [1, 1, 0] 90  [0]
                , deriv [0, 1, 1] 70  [1]
                , deriv [0, 1, 0] -5  [1, 0]
                ]
            }
        expected =
            Negative
                { zones = [ Zone 5 10 ]
                , pc = -5
                , src = [1, 0]
                }
      in
        assertEqual
        [expected]
        (find model)

    , test "Multiple zones of negative derivation should be found" <|
      let
        -- This doesn't make logical sense; it's just something with
        -- the appropriate error among the derivations.
        model =
            { segments =
                [ Segment 90 (Zone 0 10)
                , Segment 70 (Zone 5 15)
                ]
            , zones =
                [ Zone 0 5, Zone 5 10, Zone 10 15 ]
            , derivations =
                [ deriv [1, 1, 0] 90  [0]
                , deriv [0, 1, 1] 70  [1]
                , deriv [1, 0, 1] -5  [1, 0]
                ]
            }
        expected =
            Negative
                { zones = [ Zone 0 5, Zone 10 15 ]
                , pc = -5
                , src = [1, 0]
                }
      in
        assertEqual
        [expected]
        (find model)

    , test "Multiple negative derivations should all be found" <|
      let
        -- This doesn't make logical sense; it's just something with
        -- the appropriate error among the derivations.
        model =
            { segments =
                [ Segment 90 (Zone 0 10)
                , Segment 70 (Zone 5 15)
                ]
            , zones =
                [ Zone 0 5, Zone 5 10, Zone 10 15 ]
            , derivations =
                [ deriv [1, 1, 0] 90  [0]
                , deriv [0, 1, 1] 70  [1]
                , deriv [1, 0, 1] -5  [1, 0]
                , deriv [0, 1, 0] -6  [0, 1]
                ]
            }
        expected1 =
            Negative
                { zones = [ Zone 0 5, Zone 10 15 ]
                , pc = -5
                , src = [1, 0]
                }
        expected2 =
            Negative
                { zones = [ Zone 5 10 ]
                , pc = -6
                , src = [0, 1]
                }
      in
        assertEqual
        [expected1, expected2]
        (find model)

    ]

findTestForContradiction : Test
findTestForContradiction =
    suite "findTestForContradiction"

    [ test "Contradiction between two derivations should be found" <|
      let
        -- This doesn't make logical sense; it's just something with
        -- the appropriate error among the derivations.
        model =
            { segments =
                [ Segment 90 (Zone 0 10)
                , Segment 70 (Zone 5 15)
                , Segment 65 (Zone 15 20)
                ]
            , zones =
                [ Zone 0 5, Zone 5 10, Zone 10 15, Zone 15 20 ]
            , derivations =
                [ deriv [1, 1, 0, 0] 90  [0]
                , deriv [0, 1, 1, 0] 70  [1]
                , deriv [0, 0, 1, 0] 50  [1, 0]
                , deriv [0, 0, 1, 1] 65  [2]
                , deriv [0, 0, 1, 0] 35  [1, 0, 2]
                ]
            }
        expected =
            Contradiction
                { zones = [ Zone 10 15 ]
                , pcs = [50, 35]
                , src = [1, 0, 1, 0, 2]
                }
      in
        assertEqual
        [expected]
        (find model)

    , test "Distinct non-contraditions should be skipped" <|
      let
        -- This doesn't make logical sense; it's just something with
        -- the appropriate error among the derivations.
        model =
            { segments =
                [ Segment 90 (Zone 0 10)
                , Segment 70 (Zone 5 15)
                , Segment 65 (Zone 15 20)
                ]
            , zones =
                [ Zone 0 5, Zone 5 10, Zone 10 15, Zone 15 20 ]
            , derivations =
                [ deriv [1, 1, 0, 0] 90  [0]
                , deriv [0, 1, 1, 0] 70  [1]
                , deriv [0, 0, 1, 0] 50  [1, 0]
                , deriv [0, 0, 1, 1] 65  [2]
                , deriv [0, 0, 1, 0] 50  [1, 0, 2]
                ]
            }
      in
        assertEqual
        []
        (find model)

    , test "Distinct non-contraditions should be skipped while spotting multiple actual contradictions" <|
      let
        -- This doesn't make logical sense; it's just something with
        -- the appropriate error among the derivations.
        model =
            { segments =
                [ Segment 90 (Zone 0 10)
                , Segment 70 (Zone 5 15)
                , Segment 65 (Zone 15 20)
                ]
            , zones =
                [ Zone 0 5, Zone 5 10, Zone 10 15, Zone 15 20 ]
            , derivations =
                [ deriv [1, 1, 0, 0] 90  [0]
                , deriv [0, 1, 1, 0] 70  [1]
                , deriv [0, 0, 1, 0] 50  [1, 0]    -- Keep for error A
                , deriv [0, 0, 1, 1] 65  [2]
                , deriv [0, 1, 0, 1] 10  [3]       -- Keep for error B
                , deriv [0, 0, 1, 0] 50  [1, 0, 2] -- Ignore from error A
                , deriv [0, 1, 0, 1] 11  [2, 3]    -- Keep for error B
                , deriv [0, 0, 1, 0] 40  [1, 0, 3] -- Keep for error A
                ]
            }
        expected1 =
            Contradiction
                { zones = [ Zone 10 15 ]
                , pcs = [50, 40]
                , src = [1, 0, 1, 0, 3]
                }
        expected2 =
            Contradiction
                { zones = [ Zone 5 10, Zone 15 20 ]
                , pcs = [10, 11]
                , src = [3, 2, 3]
                }
      in
        assertEqual
        [expected1, expected2]
        (find model)

    ]
