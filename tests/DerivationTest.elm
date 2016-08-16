module DerivationTest exposing (all)

import Derivation exposing (..)
import Constraint exposing (Constraint)

import ElmTest exposing (..)


all : Test
all =
    suite "DerivationTest"
    [ derivationToStringTest
    , subtractTest
    , deduceOnceTest
    , deduceAllTest
    -- We'll need to bring in...
    -- model
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

deduceOnceTest : Test
deduceOnceTest =
    suite "deduceOnceTest"

    [ test "Simple deduction should work" <|
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
        (deduceOnce [der1, der2] seed)

    , test "deduceOnce should work when the seed is the larger constraint, to be subtracted from" <|
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
        (deduceOnce [der1, der2] seed)

    , test "deduceOnce should skip constraint where it can't subtract" <|
      let
        der1 = deriv [0, 1, 1] 75  [0]
        der2 = deriv [1, 1, 1] 90  [1]
        seed = deriv [1, 1, 0] 30  [2]
        -- Subtracting where we can should give this...
        res1 = deriv [0, 0, 1] 60  [1, 2]
      in
        assertEqual
        [res1]
        (deduceOnce [der1, der2] seed)

    , test "deduceOnce returns nothing if we're starting with no constraints" <|
      let
        seed = deriv [1, 1, 0] 30  [0]
      in
        assertEqual
        []
        (deduceOnce [] seed)

    , test "deduceOnce will not derive all-zero coefficients if given different constraints with same coeffs" <|
      let
        der1 = deriv [0, 1, 1] 65  [0]
        seed = deriv [0, 1, 1] 20  [1]
      in
        assertEqual
        []
        (deduceOnce [der1] seed)

    ]

deduceAllTest : Test
deduceAllTest =
    suite "deduceAllTest"

    [ test "Simple deduction of all should work" <|
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
        (deduceAll [der1, der2] seed)

    , test "deduceAll should work when the seed is the larger constraint, to be subtracted from" <|
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
        (deduceAll [der1, der2] seed)

    , test "deduceAll should skip constraint where it can't subtract" <|
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
        (deduceAll [der1, der2] seed)

    , test "deduceAll returns just the seed if we're starting with no constraints" <|
      let
        seed = deriv [1, 1, 0] 30  [0]
      in
        assertEqual
        [seed]
        (deduceAll [] seed)

    ]
