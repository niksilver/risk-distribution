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
    , putListTest
    , isNewTest
    , toReverseListTest
    , isContradictionTest
    , skipTest
    , zeroPcDerivationsTest
    , deriveOnceTest
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

putListTest : Test
putListTest =
    suite "putListTest"

    [ test "Putting nothing in an empty set should give size 1" <|
      assertEqual
      (0)
      ( empty
            |> putList []
            |> size
      )

    , test "Putting one Derivation in an empty set should give size 1" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
      in
      assertEqual
      (1)
      ( empty
            |> putList [der0]
            |> size
      )

    , test "Putting two Derivations in an empty set should give size 2" <|
      let
        der0 = deriv [1, 1, 1, 1] 100 [2, 3]
        der1 = deriv [1, 0, 0, 1] 50 [1]
      in
      assertEqual
      (2)
      ( empty
            |> putList [der0, der1]
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

toReverseListTest : Test
toReverseListTest =
    suite "toReverseListTest"

    [ test "From empty set should give empty list" <|
      assertEqual
      []
      (empty |> toReverseList)

    , test "From 3 element set should give elements reversed" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 0, 0, 1] 25 [1]
        der2 = deriv [1, 0, 1, 1] 45 [0]
      in
      assertEqual
      [der2, der1, der0]
      ( empty
            |> put der0
            |> put der1
            |> put der2
            |> toReverseList
      )

    ]

isContradictionTest : Test
isContradictionTest =
    suite "isContradictionTest"

    [ test "A Derivation should not be a contradition to the empty set" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
      in
      assertEqual
      (False)
      (empty |> isContradiction der0)

    , test "A Derivation should not be a contradition to a set which contains just itself" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
      in
      assertEqual
      (False)
      ( empty
            |> put der0
            |> isContradiction der0
      )

    , test "A Derivation should not be a contradition to a set which contains the same thing from other sources" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 0, 0, 1] 50 [0]  -- The same thing but from different sources
      in
      assertEqual
      (False)
      ( empty
            |> put der0
            |> isContradiction der1
      )

    , test "A Derivation should be a contradition to a set which contains a different percentage for the same coeffs" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 0, 0, 1] 45 [0]  -- Different percentage
      in
      assertEqual
      (True)
      ( empty
            |> put der0
            |> isContradiction der1
      )

    ]

skipTest : Test
skipTest =
    suite "skipTest"

    [ test "Should not skip a Derivation given an empty set" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
      in
      assertEqual
      (False)
      ( empty
            |> skip der0
      )

    , test "Should not skip a Derivation given an set of completely different ones" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 1, 0, 0] 20 [0]
        der2 = deriv [0, 0, 1, 1] 33 [1, 2]
        der3 = deriv [1, 0, 0, 0] 10 [1]
      in
      assertEqual
      (False)
      ( empty
            |> put der0
            |> put der1
            |> put der2
            |> skip der3
      )

    , test "Should not skip a Derivation that's a contradition to one already there" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 1, 0, 0] 20 [0]
        der2 = deriv [0, 0, 1, 1] 33 [1, 2]
        der3 = deriv [1, 1, 0, 0] 10 [1]  -- Contradicts der1
      in
      assertEqual
      (False)
      ( empty
            |> put der0
            |> put der1
            |> put der2
            |> skip der3
      )

    , test "Should skip a Derivation that's equal to one already there (even if different source)" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 1, 0, 0] 20 [0]
        der2 = deriv [0, 0, 1, 1] 33 [1, 2]
        der3 = deriv [1, 1, 0, 0] 20 [1]  -- Same as der1, apart from the source
      in
      assertEqual
      (True)
      (empty
            |> put der0
            |> put der1
            |> put der2
            |> skip der3
      )

    ]

zeroPcDerivationsTest : Test
zeroPcDerivationsTest =
    suite "zeroPcDerivationsTest"

    [ test "Trying to split no coeffs of 0% should give no nothing (although it makes no sense)" <|
      let
        der1 = deriv [] 0 [2]
      in
        assertEqual
        []
        (zeroPcDerivations der1)

    , test "Trying to split no coeffs of >0% should give no nothing (although it makes no sense)" <|
      let
        der1 = deriv [] 10 [2]
      in
        assertEqual
        []
        (zeroPcDerivations der1)

    , test "Trying to split a derivation of >0% should give no derivations" <|
      let
        der1 = deriv [0, 1, 1, 0] 10 [2]
      in
        assertEqual
        []
        (zeroPcDerivations der1)

    , test "Trying to split a derivation of >0% and one zone should give no derivations" <|
      let
        der1 = deriv [0, 1, 0, 0] 10 [2]
      in
        assertEqual
        []
        (zeroPcDerivations der1)

    , test "Trying to split a derivation of 0% and one zone should give no derivations" <|
      let
        der1 = deriv [0, 1, 0, 0] 0 [2]
      in
        assertEqual
        []
        (zeroPcDerivations der1)

    , test "Trying to split a derivation of 0% over two zones should give new derivations" <|
      let
        der1 = deriv [0, 1, 1, 0] 0 [2]
      in
        assertEqual
        [ deriv [0, 1, 0, 0] 0 [2]
        , deriv [0, 0, 1, 0] 0 [2]
        ]
        (zeroPcDerivations der1)

    ]

deriveOnceTest : Test
deriveOnceTest =
    suite "deriveOnceTest"

    [ test "Simple derivation should work" <|
      let
        der1 = deriv [1, 1, 1] 100 [0]
        der2 = deriv [0, 1, 1] 55  [1]
        seed = deriv [0, 0, 1] 30  [2]
        -- This subtracting seed from the others should give these
        -- But note they're derived from the bottom up
        res1 = deriv [0, 1, 0] 25  [1, 2]
        res2 = deriv [1, 1, 0] 70  [0, 2]

        -- So our starter set is
        set = putList [der1, der2] empty
      in
        assertEqual
        [res1, res2]
        (deriveOnce set seed)
--
--     , test "deriveOnce should work when the seed is the larger constraint, to be subtracted from" <|
--       let
--         der1 = deriv [0, 1, 1] 65  [0]
--         der2 = deriv [0, 1, 0] 20  [1]
--         seed = deriv [1, 1, 1] 80  [2]
--         -- Subtracting others from seed should give these...
--         res1 = deriv [1, 0, 0] 15  [2, 0]
--         res2 = deriv [1, 0, 1] 60  [2, 1]
--       in
--         assertEqual
--         [res1, res2]
--         (deriveOnce [der1, der2] seed)
--
--     , test "deriveOnce should skip constraint where it can't subtract" <|
--       let
--         der1 = deriv [0, 1, 1] 75  [0]
--         der2 = deriv [1, 1, 1] 90  [1]
--         seed = deriv [1, 1, 0] 30  [2]
--         -- Subtracting where we can should give this...
--         res1 = deriv [0, 0, 1] 60  [1, 2]
--       in
--         assertEqual
--         [res1]
--         (deriveOnce [der1, der2] seed)
--
--     , test "deriveOnce returns nothing if we're starting with no constraints" <|
--       let
--         seed = deriv [1, 1, 0] 30  [0]
--       in
--         assertEqual
--         []
--         (deriveOnce [] seed)
--
--     , test "deriveOnce will not derive all-zero coefficients if given different constraints with same coeffs" <|
--       let
--         der1 = deriv [0, 1, 1] 65  [0]
--         seed = deriv [0, 1, 1] 20  [1]
--       in
--         assertEqual
--         []
--         (deriveOnce [der1] seed)
--
--     , test "If multiple coeffs sum to 0 then deriveOnce should infer that each zone is 0" <|
--       let
--         der1 = deriv [1, 1, 1] 80  [0]
--         seed = deriv [1, 0, 0] 80  [1]
--         -- Subtracting gives this...
--         res1 = deriv [0, 1, 1]  0  [0, 1]
--         -- ...these multiple coeffs of 0 mean we should also infer these...
--         res2 = deriv [0, 1, 0]  0  [0, 1]
--         res3 = deriv [0, 0, 1]  0  [0, 1]
--       in
--         assertEqual
--         [res1, res2, res3]
--         (deriveOnce [der1] seed)

    ]
