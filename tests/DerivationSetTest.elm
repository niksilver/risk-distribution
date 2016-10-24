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
    , isNewTest
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

isNewTest : Test
isNewTest =
    suite "isNewTest"

    [ test "Any Derivation in an empty set should be new" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
      in
      assertEqual
      (True)
      ( empty
            |> isNew der0
      )

    , test "A Derivation should be new if it's the only member of the set" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
      in
      assertEqual
      (False)
      ( empty
            |> put der0
            |> isNew der0
      )

    , test "A Derivation that's not in a non-empty set should be new" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 0, 0, 1] 25 [1]
      in
      assertEqual
      (False)
      ( empty
            |> put der0
            |> isNew der1
      )

    , test "A Derivation that has the same coeffs as one in the set should not be new" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 0, 0, 1] 25 [1]
        der2 = deriv [1, 0, 0, 1] 45 [0]  -- Same coeffs as der0
      in
      assertEqual
      (False)
      ( empty
            |> put der0
            |> put der1
            |> isNew der2
      )

    ]
