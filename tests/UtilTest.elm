module UtilTest exposing (all)

import Util exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "UtilTest"
    [ findTest
    , findPairTest
    ]

findTest : Test
findTest =
    suite "findTest"

    [ test "Success on an empty list should yield Nothing" <|
      assertEqual
      Nothing
      (find (always (Just 1)) [])

    , test "Success on a populated list should yield something" <|
      assertEqual
      (Just 1)
      (find (always (Just 1)) [99, 98, 97])

    , test "Finding an even number among odds and evens should yield even" <|
      let
          isEven a = if (a % 2) == 0 then Just a else Nothing
      in
          assertEqual
          (Just 4)
          (find isEven [5, 9, 4, 2, 7, 10])

    , test "Finding an even number among odds should yield nothing" <|
      let
          isEven a = if (a % 2) == 0 then Just a else Nothing
      in
          assertEqual
          Nothing
          (find isEven [5, 9, 1, 5, 7])

    ]

findPairTest : Test
findPairTest =
    suite "findPairTest"

    [ test "Non-passing test on populated list should give Nothing" <|
      assertEqual
      Nothing
      (findPair (\a b -> Nothing) [1, 2, 3])

    , test "Non-passing test on empty list should give Nothing" <|
      assertEqual
      Nothing
      (findPair (\a b -> Nothing) [])

    , test "Should be able to find items 1 apart, in order" <|
      let
          oneApart a b =
              if (b == a + 1 || a == b + 1) then Just (a, b) else Nothing
      in
          assertEqual
          (Just (10, 11))
          (findPair oneApart [1, 5, 10, 8, 11, 20])

    , test "Should be able to find items 2 apart, in reverse order" <|
      let
          twoApart a b =
              if (b == a + 2 || a == b + 2) then Just (a, b) else Nothing
      in
          assertEqual
          (Just (12, 10))
          (findPair twoApart [1, 5, 12, 9, 10, 20])
 
    ]

