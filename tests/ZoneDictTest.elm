module ZoneDictTest exposing (all)

import ZoneDict exposing (..)

import Zone exposing (inf, Zone)
import Value exposing (Value(..))
import Constraint exposing (Constraint)
import Derivation exposing (Derivation)

import ElmTest exposing (..)


all : Test
all =
    suite "ZoneDictTest"
    [ getEntriesTest
    , fillAndToListTest
    ]


-- A quick way of creating a Derivation

deriv : List Int -> Int -> List Int -> Derivation
deriv coeffs pc src =
    Derivation (Constraint coeffs pc) src


-- The tests...

getEntriesTest : Test
getEntriesTest =
    suite "getEntriesTest"

    [ test "Given no coeffs we should get no entries" <|
      assertEqual
      []
      (getEntries
        [Zone 0 1]
        (deriv [] 100 [0])
      )

    , test "Given single coeff of 1 we should get one entry (1)" <|
      assertEqual
      [(Zone 5 7, Exactly 50 [3])]
      (getEntries
        [Zone 5 7]
        (deriv [1] 50 [3])
      )

    , test "Given single coeff of 1 we should get one entry (2)" <|
      assertEqual
      [(Zone 1 4, Exactly 66 [2])]
      (getEntries
        [Zone 1 4]
        (deriv [1] 66 [2])
      )

    , test "Given single coeff of 1 we should get no entries" <|
      assertEqual
      []
      (getEntries
        [Zone 5 7]
        (deriv [0] 50 [1])
      )

    , test "Given two coeffs of 0, 0 we should get no entries" <|
      assertEqual
      []
      (getEntries
        [Zone 5 7]
        (deriv [0, 0] 50 [2, 0])
      )

    , test "Given two coeffs of 1, 0 we should get entry for first only" <|
      assertEqual
      [(Zone 5 7, Exactly 30 [2, 1])]
      (getEntries
        [Zone 5 7, Zone 7 10]
        (deriv [1, 0] 30 [2, 1])
      )

    , test "Given two coeffs of 0, 1 we should get entry for second only" <|
      assertEqual
      [(Zone 7 10, Exactly 40 [4, 3])]
      (getEntries
        [Zone 5 7, Zone 7 10]
        (deriv [0, 1] 40 [4, 3])
      )

    , test "Given two coeffs of 1, 1 we should get entry for both, stating a max" <|
      assertEqual
      [(Zone 5 7, Maximum 40 [5, 2]), (Zone 7 10, Maximum 40 [5, 2])]
      (getEntries
        [Zone 5 7, Zone 7 10]
        (deriv [1, 1] 40 [5, 2])
      )

    , test "Given three coeffs of 0, 1, 1 we should get entries for second and third only, stating max" <|
      assertEqual
      [(Zone 5 7, Maximum 50 [1, 3]), (Zone 7 10, Maximum 50 [1, 3])]
      (getEntries
        [Zone 0 5, Zone 5 7, Zone 7 10]
        (deriv [0, 1, 1] 50 [1, 3])
      )
    ]

fillAndToListTest : Test
fillAndToListTest =
    suite "fillAndToListTest"

    [ test "Fill works out values, including a single negative" <|
      let
        zones =
          [ Zone 0 5, Zone 5 10, Zone 10 15 ]
        derivations =
          [ deriv [1, 1, 0] 90  [0]
          , deriv [0, 1, 1] 70  [1]
          , deriv [0, 1, 0] -5  [1, 0]
          ]
      in
        assertEqual
        [ (Zone 0 5,   Maximum 90 [0])
        , (Zone 5 10,  Exactly -5 [1, 0])
        , (Zone 10 15, Maximum 70 [1])
        ]
        (fill zones derivations |> toList)

    , test "Fill works out values, including multiple negatives from single derivation" <|
      let
        zones =
          [ Zone 0 5, Zone 5 10, Zone 10 15 ]
        derivations =
          [ deriv [1, 1, 0] 90  [0]
          , deriv [0, 1, 1] 70  [1]
          , deriv [1, 0, 1] -5  [1, 0]
          ]
      in
        assertEqual
        [ (Zone 0 5,   Maximum -5 [1, 0])
        , (Zone 5 10,  Maximum 70 [1])
        , (Zone 10 15, Maximum -5 [1, 0])
        ]
        (fill zones derivations |> toList)

    , test "Fill works out values, including multiple negatives from two derivations" <|
      let
        zones =
          [ Zone 0 5, Zone 5 10, Zone 10 15 ]
        derivations =
          [ deriv [1, 1, 0] 90  [0]
          , deriv [0, 1, 1] 70  [1]
          , deriv [1, 0, 1] -5  [1, 0]
          , deriv [0, 1, 0] -6  [0, 1]
          ]
      in
        assertEqual
        [ (Zone 0 5,   Maximum -5 [1, 0])
        , (Zone 5 10,  Exactly -6 [0, 1])
        , (Zone 10 15, Maximum -5 [1, 0])
        ]
        (fill zones derivations |> toList)

    , test "Fill works out values, including a contradiction" <|
      let
        zones =
          [ Zone 0 5, Zone 5 10, Zone 10 15, Zone 15 20 ]
        derivations =
          [ deriv [1, 1, 0, 0] 90  [0]
          , deriv [0, 1, 1, 0] 70  [1]
          , deriv [0, 0, 1, 0] 50  [1, 0]
          , deriv [0, 0, 1, 1] 65  [2]
          , deriv [0, 0, 1, 0] 35  [1, 0, 2]
          ]
      in
        assertEqual
        [ (Zone 0 5,   Maximum 90 [0])
        , (Zone 5 10,  Maximum 70 [1])
        , (Zone 10 15, Contradiction [1, 0, 2]) -- Rationalised from [1, 0, 1, 0, 2]
        , (Zone 15 20, Maximum 65 [2])
        ]
        (fill zones derivations |> toList)

    , test "Fill won't be fooled by distinct non-contradictions" <|
      let
        zones =
          [ Zone 0 5, Zone 5 10, Zone 10 15, Zone 15 20 ]
        derivations =
          [ deriv [1, 1, 0, 0] 90  [0]
          , deriv [0, 1, 1, 0] 70  [1]
          , deriv [0, 0, 1, 0] 50  [1, 0]    -- One way of deriving Zone 10 15
          , deriv [0, 0, 1, 1] 65  [2]
          , deriv [0, 0, 1, 0] 50  [1, 0, 2] -- Another way of deriving Zone 10 15
          ]
      in
        assertEqual
        [ (Zone 0 5,   Maximum 90 [0])
        , (Zone 5 10,  Maximum 70 [1])
        , (Zone 10 15, Exactly 50 [1, 0])
        , (Zone 15 20, Maximum 65 [2])
        ]
        (fill zones derivations |> toList)

    , test "Fill skips distinct non-contraditions while spotting multiple actual contradictions" <|
      let
        zones =
          [ Zone 0 5, Zone 5 10, Zone 10 15, Zone 15 20 ]
        derivations =
          [ deriv [1, 1, 0, 0] 90  [0]
          , deriv [0, 1, 1, 0] 70  [1]
          , deriv [0, 0, 1, 0] 50  [1, 0]    -- Keep for error
          , deriv [0, 0, 1, 1] 65  [2]
          , deriv [0, 1, 0, 1] 10  [3]
          , deriv [0, 0, 1, 0] 50  [1, 0, 2] -- Ignore from error
          , deriv [0, 1, 0, 1] 11  [2, 3]
          , deriv [0, 0, 1, 0] 40  [1, 0, 3] -- Keep for error
          ]
      in
        assertEqual
        [ (Zone 0 5,   Maximum 90 [0])
        , (Zone 5 10,  Maximum 10 [3])
        , (Zone 10 15, Contradiction [1, 0, 3]) -- Rationalised from [1, 0, 1, 0, 3]
        , (Zone 15 20, Maximum 10 [3])
        ]
        (fill zones derivations |> toList)

    ]
