module DerivationTest exposing (all)

import Derivation exposing (..)
import Constraints exposing (Constraint)

import ElmTest exposing (..)


all : Test
all =
    suite "DerivationTest"
    [ derivationToStringTest
    , subtractTest
    -- We'll need to bring in...
    -- deriveOnce
    -- deriveAll
    -- model
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

subtractTest : Test
subtractTest =
    suite "subtractTest"

    [ test "Basic subtraction should work" <|
      assertEqual
      (Derivation (Constraint [1, 3, -3] 12) [2, 1])
      (subtract
        (Derivation (Constraint [2, 3, 4] 90) [2])
        (Derivation (Constraint [1, 0, 7] 78) [1])
      )

    , test "Longer subtract shorter should ignore extra coefficients" <|
      assertEqual
      (Derivation (Constraint [-2, 2] 4) [3, 0, 1])
      (subtract
        (Derivation (Constraint [2, 3, 4, 1] 10) [3])
        (Derivation (Constraint [4, 1]        6) [0, 1])
      )

    , test "Shorter subtract longer should ignore extra coefficients" <|
      assertEqual
      (Derivation (Constraint [2, -2] 3) [4])
      (subtract
        (Derivation (Constraint [4, 1]       20) [])
        (Derivation (Constraint [2, 3, 4, 5] 17) [4])
      )

    ]
