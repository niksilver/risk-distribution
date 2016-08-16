module DerivationTest exposing (all)

import Derivation exposing (..)
import Constraints exposing (Constraint)

import ElmTest exposing (..)


all : Test
all =
    suite "DerivationTest"
    [ derivationToStringTest
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
