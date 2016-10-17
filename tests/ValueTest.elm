module ValueTest exposing (all)

import Value exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "ValueTest"
    [ percentTest
    , combineTest
    , rationaliseTest
    ]


percentTest : Test
percentTest =
    suite "percentTest"

    [ test "Percent of a Maximum should be just that value" <|
      assertEqual
      (Just 11)
      (Maximum 11 [5, 6] |> percent)

    , test "Percent of an Exactly should be just that value" <|
      assertEqual
      (Just 22)
      (Exactly 22 [3, 2, 1] |> percent)

    , test "Percent of a Contradiction should be Nothing" <|
      assertEqual
      Nothing
      (Contradiction [0, 1] |> percent)

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
