module ErrorTest exposing (all)

import Error exposing (..)
import Zone exposing (Zone)
import Constraint exposing (Segment, Constraint)
import Derivation exposing (Derivation)

import ElmTest exposing (..)

all : Test
all =
    suite "ErrorTest"
    [ findTest
    ]

-- A quick way of creating a Derivation

deriv : List Int -> Int -> List Int -> Derivation
deriv coeffs pc src =
    Derivation (Constraint coeffs pc) src

-- The tests...

findTest : Test
findTest =
    suite "findTest"

    [ test "Zone of negative value should be found" <|
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
            Negative { zones = [ Zone 5 10 ], pc = -5, src = [1, 0] }
      in
        assertEqual
        [expected]
        (find model)

    ]
