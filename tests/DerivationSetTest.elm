module DerivationSetTest exposing (all)

import DerivationSet exposing (..)

import Constraint as Cons exposing (Constraint)
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
    , introducesErrorTest
    , skipTest
    , zeroPcDerivationsTest
    , deriveOnceTest
    , deriveAllTest
    , deriveAllWithListsTest
    , lastIsAContradictionTest
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

introducesErrorTest : Test
introducesErrorTest =
    suite "introducesErrorTest"

    [ test "A Derivation of a positive %age should not introduce an error to the empty set" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
      in
      assertEqual
      (False)
      (empty |> introducesError der0)

    , test "A Derivation of a negative %age should introduce an error to some set" <|
      let
        der0 = deriv [1, 0, 0, 1] -50 [2, 3]
      in
      assertEqual
      (True)
      (empty |> introducesError der0)

    , test "A Derivation of a should not introduce an error to a set which contains just itself" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
      in
      assertEqual
      (False)
      ( empty
            |> put der0
            |> introducesError der0
      )

    , test "A Derivation should not introduce an error to a set which contains the same thing from other sources" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 0, 0, 1] 50 [0]  -- The same thing but from different sources
      in
      assertEqual
      (False)
      ( empty
            |> put der0
            |> introducesError der1
      )

    , test "A Derivation should introduce an error to a set which contains a different percentage for the same coeffs" <|
      let
        der0 = deriv [1, 0, 0, 1] 50 [2, 3]
        der1 = deriv [1, 0, 0, 1] 45 [0]  -- Different percentage
      in
      assertEqual
      (True)
      ( empty
            |> put der0
            |> introducesError der1
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
        res1 = deriv [1, 1, 0] 70  [0, 2]
        res2 = deriv [0, 1, 0] 25  [1, 2]
        -- But they're derived from the bottom up so will come out in reverse order

        -- So our starter set is
        set = putList [der1, der2] empty
      in
        assertEqual
        [res2, res1]
        (deriveOnce set seed)

    , test "deriveOnce should work when the seed is the larger constraint, to be subtracted from" <|
      let
        der1 = deriv [0, 1, 1] 65  [0]
        der2 = deriv [0, 1, 0] 20  [1]
        seed = deriv [1, 1, 1] 80  [2]
        -- Subtracting others from seed should give these
        -- But they're derived from the bottom up so will come out in reverse order
        res1 = deriv [1, 0, 0] 15  [2, 0]
        res2 = deriv [1, 0, 1] 60  [2, 1]

        -- So our starter set is
        set = putList [der1, der2] empty
      in
        assertEqual
        [res2, res1]
        (deriveOnce set seed)

    , test "deriveOnce should skip constraint where it can't subtract" <|
      let
        der1 = deriv [0, 1, 1] 75  [0]
        der2 = deriv [1, 1, 1] 90  [1]
        seed = deriv [1, 1, 0] 30  [2]
        -- Subtracting where we can should give this...
        res1 = deriv [0, 0, 1] 60  [1, 2]

        -- So our starter set is
        set = putList [der1, der2] empty
      in
        assertEqual
        [res1]
        (deriveOnce set seed)

    , test "deriveOnce returns nothing if we're starting with no constraints" <|
      let
        seed = deriv [1, 1, 0] 30  [0]
      in
        assertEqual
        []
        (deriveOnce empty seed)

    , test "deriveOnce will not derive all-zero coefficients if given different constraints with same coeffs" <|
      let
        der1 = deriv [0, 1, 1] 65  [0]
        seed = deriv [0, 1, 1] 20  [1]

        -- So our starter set is
        set = put der1 empty
      in
        assertEqual
        []
        (deriveOnce set seed)

    , test "If multiple coeffs sum to 0 then deriveOnce should infer that each zone is 0" <|
      let
        der1 = deriv [1, 1, 1] 80  [0]
        seed = deriv [1, 0, 0] 80  [1]
        -- Subtracting gives this...
        res1 = deriv [0, 1, 1]  0  [0, 1]
        -- ...these multiple coeffs of 0 mean we should also infer these
        -- which will come out in the right order
        res2 = deriv [0, 1, 0]  0  [0, 1]
        res3 = deriv [0, 0, 1]  0  [0, 1]
        -- But they will come out in reverse order

        -- So our starter set is
        set = put der1 empty
      in
        assertEqual
        [res1, res2, res3]
        (deriveOnce set seed)

    ]

deriveAllTest : Test
deriveAllTest =
    suite "deriveAllTest"

    [ test "Simple derivation of all should work" <|
      let
        der1 = deriv [1, 1, 1] 100 [0]
        der2 = deriv [0, 1, 1] 55  [1]
        seed = deriv [0, 0, 1] 30  [2]
        -- This subtracting seed from the others should give these (these are reversed)...
        res1 = deriv [1, 1, 0] 70  [0, 2]
        res2 = deriv [0, 1, 0] 25  [1, 2]
        -- And res2 should give this...
        res3 = deriv [1, 0, 1] 75  [0, 1, 2]
        -- And res1 and res2 should give this
        res4 = deriv [1, 0, 0] 45  [0, 2, 1, 2]

        -- Our starter set is
        set = putList [der1, der2] empty
      in
        assertEqual
        [der1, der2, seed, res2, res1, res3, res4]
        (deriveAll set seed
            |> Result.withDefault empty
            |> toReverseList
            |> List.reverse
        )

    , test "deriveAll should work when the seed is the larger constraint, to be subtracted from" <|
      let
        der1 = deriv [0, 1, 1] 65  [0]
        der2 = deriv [0, 1, 0] 20  [1]
        seed = deriv [1, 1, 1] 80  [2]
        -- This subtracting others from seed should give these (reversed)...
        res1 = deriv [1, 0, 0] 15  [2, 0]
        res2 = deriv [1, 0, 1] 60  [2, 1]
        -- Then subtracting these two gives...
        res3 = deriv [0, 0, 1] 45  [2, 1, 2, 0]
        -- And subtracting that from the seed gives
        res4 = deriv [1, 1, 0] 35  [2, 2, 1, 2, 0]

        -- Our starter set is...
        set = putList [der1, der2] empty
      in
        assertEqual
        [der1, der2, seed, res2, res1, res3, res4]
        (deriveAll set seed
            |> Result.withDefault empty
            |> toReverseList
            |> List.reverse
        )

    , test "deriveAll should skip constraint where it can't subtract" <|
      let
        der1 = deriv [0, 1, 1] 75  [0]
        der2 = deriv [1, 1, 1] 90  [1]
        seed = deriv [1, 1, 0] 30  [2]
        -- Subtracting where we can should give this...
        res1 = deriv [0, 0, 1] 60  [1, 2]
        -- And that should in turn give this...
        res2 = deriv [0, 1, 0] 15  [0, 1, 2]
        -- And from that we can derive these (in reverse)...
        res3 = deriv [1, 0, 1] 75  [1, 0, 1, 2]
        res4 = deriv [1, 0, 0] 15  [2, 0, 1, 2]

        -- Our starter set is...
        set = putList [der1, der2] empty
      in
        assertEqual
        [der1, der2, seed, res1, res2, res4, res3]
        (deriveAll set seed
            |> Result.withDefault empty
            |> toReverseList
            |> List.reverse
        )

    , test "deriveAll returns just the seed if we're starting with no constraints" <|
      let
        seed = deriv [1, 1, 0] 30  [0]
      in
        assertEqual
        [seed]
        (deriveAll empty seed
            |> Result.withDefault empty
            |> toReverseList
            |> List.reverse
        )

    , test "If a 0% is derived then all constituent zones should be 0%" <|
      let
        der0 = deriv [1, 1, 1, 1] 100  [0]
        der1 = deriv [0, 1, 1, 0] 100  [1]
        -- Subtracting should give...
        res1 = deriv [1, 0, 0, 1]   0  [0, 1]
        -- And this should yield this (not reversed, because they're 0% extractions from the above)...
        res2 = deriv [1, 0, 0, 0]   0  [0, 1]
        res3 = deriv [0, 0, 0, 1]   0  [0, 1]
        --- ...and there will be some other results which we'll ignore

        -- Our starter set is...
        set = put der0 empty
      in
        assertEqual
        [der0, der1, res1, res2, res3]
        (deriveAll set der1
            |> Result.withDefault empty
            |> toReverseList
            |> List.reverse
            |> List.take 5
        )

    , test "deriveAll should terminate if there's an inconsistent 'between' derivation" <|
      -- This error derives from entering the following constraints:
      -- There's a 100% chance it's between 0 and 10  [1]
      -- There's a  50% chance it's between 2 and 8   [2]
      -- There's a  40% chance it's between 2 and 8   [3]
      -- (Plus the implicit 100% chance it's between -inf and inf)
      -- So the zones are: -inf to 0,  0 to 2,  2 to 8,  8 to 10,  10 to inf
      let
        der0 = deriv [1, 1, 1, 1, 1] 100  [0]
        der1 = deriv [0, 1, 1, 1, 0] 100  [1]
        der2 = deriv [0, 0, 1, 0, 0]  50  [2]
        der3 = deriv [0, 0, 1, 0, 0]  40  [3]

        -- Our starter set is...
        set = put der0 empty
        res1 = deriveAll set der1
        res2 = Result.andThen res1 (\res -> deriveAll res der2)
      in
        assertEqual
        Nothing
        (Result.andThen res2 (\res -> deriveAll res der3)
            |> Result.toMaybe
        )

    , test "If there's an inconsistent 'between' derivation we should return the problem derivation" <|
      -- This error derives from entering the following constraints:
      -- There's a 100% chance it's between 0 and 10  [1]
      -- There's a  50% chance it's between 2 and 8   [2]
      -- There's a  40% chance it's between 2 and 8   [3]
      -- (Plus the implicit 100% chance it's between -inf and inf)
      -- So the zones are: -inf to 0,  0 to 2,  2 to 8,  8 to 10,  10 to inf
      let
        der0 = deriv [1, 1, 1, 1, 1] 100  [0]
        der1 = deriv [0, 1, 1, 1, 0] 100  [1]
        der2 = deriv [0, 0, 1, 0, 0]  50  [2]
        der3 = deriv [0, 0, 1, 0, 0]  40  [3]

        -- Our starter set is...
        set = put der0 empty
        res1 = deriveAll set der1
        res2 = Result.andThen res1 (\res -> deriveAll res der2)
      in
        assertEqual
        (Err der3)
        (Result.andThen res2 (\res -> deriveAll res der3))

    , test "deriveAll should work quickly in this case (previously it took ages)" <|
      -- This isn't testing anything really, we just want to run it and see
      -- that it doesn't take ages. In an early (purely list-based)
      -- implementation it took 8 seconds.
      -- We end up with 127 derivations.
      let
        --              0 25 30 50 70  C inf
        der0 = deriv [1, 1, 1, 1, 1, 1, 1] 100  [0]
        der1 = deriv [0, 1, 1, 1, 1, 1, 0] 100  [1]
        der2 = deriv [1, 1, 0, 0, 0, 0, 0]  50  [2]
        der3 = deriv [0, 0, 0, 0, 1, 1, 1]  10  [3]
        der4 = deriv [0, 0, 0, 1, 1, 0, 0]   0  [4]

        res0 = put der0 empty
        res1 = deriveAll res0 der1
        res2 = Result.andThen res1 (\res -> deriveAll res der2)
        res3 = Result.andThen res2 (\res -> deriveAll res der3)
      in
        assertEqual
        (Ok 127)
        (Result.andThen res3 (\res -> deriveAll res der4)
            |> Result.map size
        )

    ]

deriveAllWithListsTest : Test
deriveAllWithListsTest =
    suite "deriveAllWithListsTest"

    [ test "Basic derivation with lists should work" <|
      let
        der1 = deriv [0, 1, 1] 75  [0]
        der2 = deriv [1, 1, 1] 90  [1]
        seed = deriv [1, 1, 0] 30  [2]
        -- Subtracting where we can should give this...
        res1 = deriv [0, 0, 1] 60  [1, 2]
        -- And that should in turn give this...
        res2 = deriv [0, 1, 0] 15  [0, 1, 2]
        -- And from that we can derive these (in reverse)...
        res3 = deriv [1, 0, 1] 75  [1, 0, 1, 2]
        res4 = deriv [1, 0, 0] 15  [2, 0, 1, 2]
      in
        assertEqual
        (Ok [der1, der2, seed, res1, res2, res4, res3])
        (deriveAllWithLists [der1, der2] seed)

    ]

lastIsAContradictionTest : Test
lastIsAContradictionTest =
    suite "lastIsAContradictionTest"

    [ test "Should be False for an empty derivation set" <|
      assertEqual
      (False)
      (lastIsAContradiction empty)

    , test "Should be False for an singleton derivation set" <|
      let
        der1 = deriv [0, 1, 1, 0] 10 [2]
        set = put der1 empty
      in
        assertEqual
        (False)
        (lastIsAContradiction set)

    , test "Should be False for a set with two different derivations" <|
      let
        der1 = deriv [0, 1, 1, 0] 10 [2]
        der2 = deriv [1, 1, 1, 0] 20 [1]
        set = putList [der1, der2] empty
      in
        assertEqual
        (False)
        (lastIsAContradiction set)

    , test "Should be True if the last two in a set are contradictions" <|
      let
        der1 = deriv [0, 1, 1, 0] 10 [2]
        der2 = deriv [1, 1, 1, 0] 20 [1]
        der3 = deriv [1, 1, 1, 0] 30 [0, 1]
        set = putList [der1, der2, der3] empty
      in
        assertEqual
        (True)
        (lastIsAContradiction set)

    , test "Should be True if first and last in a set are contradictions" <|
      let
        der1 = deriv [1, 1, 1, 0] 20 [1]
        der2 = deriv [0, 1, 1, 0] 10 [2]
        der3 = deriv [1, 1, 1, 0] 30 [0, 1]
        set = putList [der1, der2, der3] empty
      in
        assertEqual
        (True)
        (lastIsAContradiction set)

    , test "Should be False if there are contradictions, but the last isn't one of them" <|
      let
        der1 = deriv [1, 1, 1, 0] 20 [1]
        der2 = deriv [1, 1, 1, 0] 30 [0, 1]
        der3 = deriv [0, 1, 1, 0] 10 [2]
        set = putList [der1, der2, der3] empty
      in
        assertEqual
        (False)
        (lastIsAContradiction set)

    , test "Should be False if the last two in a set are equal apart from their sources" <|
      let
        der1 = deriv [0, 1, 1, 0] 10 [2]
        der2 = deriv [1, 1, 1, 0] 30 [1]
        der3 = deriv [1, 1, 1, 0] 30 [0, 1]
        set = putList [der1, der2, der3] empty
      in
        assertEqual
        (False)
        (lastIsAContradiction set)

    ]
