module ZoneDictTest exposing (all)

import ZoneDict exposing (..)

import Zone exposing (inf, Zone)
import Constraint exposing (Constraint)
import Derivation exposing (Derivation)
import Util

import ElmTest exposing (..)

all : Test
all =
    suite "ZoneDictTest"
    [ toValueFloattTest
    , getEntriesTest
    , combineTest
    , rationaliseTest
    , fillAndToListTest
    , taperZoneWidthTest
    , toRangeTest
    ]

-- A quick way of creating a Derivation

deriv : List Int -> Int -> List Int -> Derivation
deriv coeffs pc src =
    Derivation (Constraint coeffs pc) src

-- The tests...

toValueFloattTest : Test
toValueFloattTest =
    suite "toValueFloattTest"

    [ test "toValueFloat exact should work (1)" <|
      assertEqual
      (Exactly 100.0 [4, 5])
      (Exactly 100 [4, 5] |> toValueFloat)

    , test "toValueFloat exact should work (2)" <|
      assertEqual
      (Exactly 80.0 [3, 0])
      (Exactly 80 [3, 0] |> toValueFloat)

    , test "toValueFloat max should work (1)" <|
      assertEqual
      (Maximum 99.0 [0])
      (Maximum 99 [0] |> toValueFloat)

    , test "toValueFloat max should work (2)" <|
      assertEqual
      (Maximum 20.0 [1, 2])
      (Maximum 20 [1, 2] |> toValueFloat)

    ]

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

-- Values to combine:
-- Exact x with:
--   Exact (== x), Exact (/= x),
--   Max (< x), Max (== x), Max (> x),
--   Contradiction
-- Maximum x with:
--   Exact (< x), Exact (== x), Exact (> x),
--   Max (< x), Max (== x), Max (> x),
--   Contradiction
-- Contradiction with:
--   Exact (anything),
--   Max (anything),
--   Contradiction (anything)

combineTest : Test
combineTest =
    suite "combineTest"

    [ -- First param is Exactly...

      test "Exactly (x) with Exactly (x) gives same Exactly (x)" <|
      assertEqual
      (Exactly 55 [4, 3])
      (combine
        (Exactly 55 [4, 3])
        (Exactly 55 [5, 6])
      )

    , test "Exactly (x) with Exactly (/= x) gives Contradiction" <|
      assertEqual
      (Contradiction [4, 3, 5, 6])
      (combine
        (Exactly 50 [4, 3])
        (Exactly 70 [5, 6])
      )

    , test "Exactly (x) with Maximum (< x) gives Contradiction" <|
      assertEqual
      (Contradiction [4, 3, 6, 7])
      (combine
        (Exactly 50 [4, 3])
        (Maximum 45 [6, 7])
      )

    , test "Exactly (x) with Maximum (== x) gives Exactly (x)" <|
      assertEqual
      (Exactly 50 [4, 3])
      (combine
        (Exactly 50 [4, 3])
        (Maximum 50 [6, 7])
      )

    , test "Exactly (x) with Maximum (> x) gives Exactly (x)" <|
      assertEqual
      (Exactly 60 [1, 3])
      (combine
        (Exactly 60 [1, 3])
        (Maximum 65 [6, 7])
      )

    , test "Exactly (x) with Contradiction gives same Contradiction" <|
      assertEqual
      (Contradiction [6, 7])
      (combine
        (Exactly 60 [1, 3])
        (Contradiction [6, 7])
      )

    -- First param is Maximum...

    , test "Maximum (x) with Exactly (< x) gives Exactly" <|
      assertEqual
      (Exactly 45 [4, 3])
      (combine
        (Maximum 50 [6, 7])
        (Exactly 45 [4, 3])
      )

    , test "Maximum (x) with Exactly (== x) gives Exactly" <|
      assertEqual
      (Exactly 50 [4, 3])
      (combine
        (Maximum 50 [6, 7])
        (Exactly 50 [4, 3])
      )

    , test "Maximum (x) with Exactly (> x) gives Contradiction" <|
      assertEqual
      (Contradiction [6, 7, 4])
      (combine
        (Maximum 50 [6, 7])
        (Exactly 60 [4])
      )

    , test "Maximum (x) with Maximum (< x) gives Maximum (< x)" <|
      assertEqual
      (Maximum 35 [4, 2])
      (combine
        (Maximum 50 [6, 7])
        (Maximum 35 [4, 2])
      )

    , test "Maximum (x) with Maximum (== x) gives Maximum (x)" <|
      assertEqual
      (Maximum 35 [6, 7])
      (combine
        (Maximum 35 [6, 7])
        (Maximum 35 [4, 2])
      )

    , test "Maximum (x) with Maximum (> x) gives Maximum (x)" <|
      assertEqual
      (Maximum 35 [6, 7])
      (combine
        (Maximum 35 [6, 7])
        (Maximum 40 [4, 2])
      )

    , test "Maximum (x) with Contradiction gives same Contradiction" <|
      assertEqual
      (Contradiction [4, 1, 2])
      (combine
        (Maximum 35 [6, 7])
        (Contradiction [4, 1, 2])
      )

    -- First paramater is Contradiction

    , test "Contradiction with Exactly (x) gives same Contradiction" <|
      assertEqual
      (Contradiction [4, 1, 2])
      (combine
        (Contradiction [4, 1, 2])
        (Exactly 35 [6, 7])
      )

    , test "Contradiction with Maximum gives same Contradiction" <|
      assertEqual
      (Contradiction [4, 1, 2])
      (combine
        (Contradiction [4, 1, 2])
        (Maximum 35 [6, 7])
      )

    , test "Contradiction with Contradiction gives union Contradiction" <|
      assertEqual
      (Contradiction [4, 1, 2, 6, 4])
      (combine
        (Contradiction [4, 1, 2])
        (Contradiction [6, 4])
      )

    ]

rationaliseTest : Test
rationaliseTest =
    suite "rationaliseTest"

    [ test "Rationalising Maximum 0 should give Exactly 0 with deduped sources" <|
      assertEqual
      (Exactly 0 [5, 1])
      (Maximum 0 [5, 5, 1] |> rationalise)

    , test "Rationalising Maximum 1 should give the same thing with deduped sources" <|
      assertEqual
      (Maximum 1 [2, 3])
      (Maximum 1 [2, 3, 3] |> rationalise)

    , test "Rationalising Maximum -1 should give the same thing with deduped sources" <|
      assertEqual
      (Maximum -1 [5, 1])
      (Maximum -1 [5, 1, 5] |> rationalise)

    , test "Rationalising Exactly should give the same thing with deduped sources" <|
      assertEqual
      (Exactly 10 [4, 2])
      (Exactly 10 [4, 2, 2] |> rationalise)

    , test "Rationalising Contradiction should give the same thing with deduped sources" <|
      assertEqual
      (Contradiction [4, 2])
      (Contradiction [4, 2, 4] |> rationalise)

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

taperZoneWidthTest : Test
taperZoneWidthTest =
    suite "taperZoneWidthTest"

    [ test "Equal %ages should give width which is a constant bigger (1)" <|
      assertEqual
      (4 * taperFactor)
      (taperZoneWidth 50 4 50)

    , test "Equal %ages should give width which is a constant bigger (2)" <|
      assertEqual
      (10 * taperFactor)
      (taperZoneWidth 20 10 20)

    , test "Zone of half the prob should give width which is half a constant bigger (1)" <|
      assertEqual
      (6 * taperFactor / 2)
      (taperZoneWidth 15 6 30)

    , test "Zone of half the prob should give width which is half a constant bigger (2)" <|
      assertEqual
      (10 * taperFactor / 2)
      (taperZoneWidth 20 10 40)

    , test "Zone of three times the prob should give width which is 3 x constant bigger (1)" <|
      assertEqual
      (50 * taperFactor * 3)
      (taperZoneWidth 15 50 5)

    , test "Zone of three times the prob should give width which is 3 x constant bigger (2)" <|
      assertEqual
      (4 * taperFactor * 3)
      (taperZoneWidth 30 4 10)

    , test "Zone of zero prob should never be used, but should give finite width if it is" <|
      assertEqual
      (True)
      (taperZoneWidth 30 4 0 |> Util.isFinite)

    , test "Zone of width should never be used, but should give finite width if it is" <|
      assertEqual
      (True)
      (taperZoneWidth 30 4 0 |> Util.isFinite)

    ]

toRangeTest : Test
toRangeTest =
    suite "toRangeTest"

    [ test "No entries should give default range" <|
      assertEqual
      (-1, 1)
      (toRange [])

    -- Zones which are all finite

    , test "Single finite zone should give that range" <|
      assertEqual
      (-10, 6)
      (toRange [(Zone -10 6, Exactly 80 [3,2])])

    , test "Two finite zones should give full range" <|
      assertEqual
      (0, 21)
      (toRange
        [ (Zone 0 8, Maximum 40 [1,2])
        , (Zone 8 21, Maximum 60 [3,1])
        ]
      )

    , test "Three finite zones should give full range" <|
      assertEqual
      (-10, 10)
      (toRange
        [ (Zone -10 -2, Maximum 20 [0, 1])
        , (Zone -2 1, Maximum 50 [3,1])
        , (Zone 1 10, Exactly 30 [2])
        ]
      )

    -- Zones from -inf to a finite number: Single zone

    , test "Single (-inf, +ve) should give range reflecting round y-axis" <|
      assertEqual
      (-4, 4)
      (toRange [(Zone -inf 4, Maximum 100 [1,2])])

    , test "Single (-inf, -ve) should give range twice further to the left" <|
      assertEqual
      (-26, -13)
      (toRange [(Zone -inf -13, Maximum 100 [0])])

    , test "Single (-inf, 0) should give default min" <|
      assertEqual
      (-1, 0)
      (toRange [(Zone -inf 0, Exactly 80 [2, 0])])

    -- Zones from -inf to a finite number: Two zones

    -- , test "Two zones (-inf, x) and (x, y) should be sized relative to their %ages and the taper-factor (1)" <|
    --   assertEqual
    --   (???, 10)
    --   (toRange
    --     [ (Zone -inf -2, Maximum 20 [0, 1])
    --     , (Zone -2 10, Maximum 50 [3,1])
    --     ]
    --   )

    -- Zones from -inf to a finite number: Three-plus zones
    -- Zones from finite number to inf: Single zone
    -- Zones from finite number to inf: Two zones
    -- Zones from finite number to inf: Three-plus zones
    -- Zones from -inf to inf: Single zone
    -- Zones from -inf to inf: Two zones
    -- Zones from -inf to inf: Three zones
    -- Zones from -inf to inf: Four-plus zones

    ]
