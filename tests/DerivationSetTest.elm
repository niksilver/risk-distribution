module DerivationSetTest exposing (all)

import DerivationSet exposing (..)

import Constraint exposing (Constraint)
import Derivation exposing (Derivation)

import ElmTest exposing (..)


all : Test
all =
    suite "DerivationSetTest"
    [ sizeAndEmptyTest
    , sizeAndPutTest
    ]


-- A quick way of creating a Derivation

deriv : List Int -> Int -> List Int -> Derivation
deriv coeffs pc src =
    Derivation (Constraint coeffs pc) src


-- The tests

sizeAndEmptyTest : Test
sizeAndEmptyTest =
    suite "sizeAndEmptyTest"

    [ test "empty set should have zero size" <|
      assertEqual
      0
      (size empty)

    ]

sizeAndPutTest : Test
sizeAndPutTest =
    suite "sizeAndPutTest"

    [ test "Putting one Derivation in an empty set should give size 1" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
      in
      assertEqual
      (1)
      ( empty
            |> put der0
            |> size
      )

    , test "Putting two different Derivations in an empty set should give size 2" <|
      let
        der0 = deriv [1, 1, 1, 1] 100 [2, 3]
        der1 = deriv [1, 0, 0, 1] 50 [1]
      in
      assertEqual
      (2)
      ( empty
            |> put der0
            |> put der1
            |> size
      )

    , test "Putting two Derivations in an empty set (which coeffs equal) should give size 1" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 0, 0, 1] 25 [1]
      in
      assertEqual
      (1)
      ( empty
            |> put der0
            |> put der1
            |> size
      )

    ]
