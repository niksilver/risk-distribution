module DerivationTest exposing (all)

import Derivation exposing (..)

import Segment exposing (Segment)
import Constraint as Cons exposing (Constraint)
import Zone exposing (inf, Zone)

import ElmTest exposing (..)


all : Test
all =
    suite "DerivationTest"
    [ derivationToStringTest
    , subtractTest
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
