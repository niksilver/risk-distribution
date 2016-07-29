module UtilTest exposing (all)

import Util exposing (..)

import String

import ElmTest exposing (..)

all : Test
all =
    suite "UtilTest"
    [ findTest
    , findPairTest
    , slidingTest
    , bracketTest
    , bracketMapTest
    , spliceOneTest
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

slidingTest : Test
slidingTest =
    suite "slidingTest"

    [ test "Sliding size 1 over empty list should give empty list" <|
      assertEqual
      []
      (sliding 1 [])

    , test "Sliding size 1 over singleton list should that in a list" <|
      assertEqual
      [[4]]
      (sliding 1 [4])

    , test "Sliding size 1 over list length two should yield singleton lists" <|
      assertEqual
      [[8], [9]]
      (sliding 1 [8, 9])

    , test "Sliding size 1 over list length three should yield singleton lists" <|
      assertEqual
      [[7], [8], [9]]
      (sliding 1 [7, 8, 9])

    -- Sliding size 3...

    , test "Sliding size 2 over empty list should yield empty list" <|
      assertEqual
      []
      (sliding 2 [])

    , test "Sliding size 2 over list length one should yield empty list" <|
      assertEqual
      []
      (sliding 2 [4])

    , test "Sliding size 2 over list length two should those in a list" <|
      assertEqual
      [[8, 9]]
      (sliding 2 [8, 9])

    , test "Sliding size 2 over list length three should yield two pairs" <|
      assertEqual
      [[7, 8], [8, 9]]
      (sliding 2 [7, 8, 9])

    , test "Sliding size 2 over list length four should yield three pairs" <|
      assertEqual
      [[6, 7], [7, 8], [8, 9]]
      (sliding 2 [6, 7, 8, 9])

    -- Sliding size 3...

    , test "Sliding size 3 over empty list should yield empty list" <|
      assertEqual
      []
      (sliding 3 [])

    , test "Sliding size 3 over list length one should yield empty list" <|
      assertEqual
      []
      (sliding 3 [4])

    , test "Sliding size 3 over list length two should yield empty list" <|
      assertEqual
      []
      (sliding 3 [8, 9])

    , test "Sliding size 3 over list length three should those in a list" <|
      assertEqual
      [[7, 8, 9]]
      (sliding 3 [7, 8, 9])

    , test "Sliding size 3 over list length 4 should yield list of two" <|
      assertEqual
      [[6, 7, 8], [7, 8, 9]]
      (sliding 3 [6, 7, 8, 9])

    ]

bracketTest : Test
bracketTest =
    suite "bracketTest"

    [ test "Bracketing the empty list should give the empty list" <|
      assertEqual
      []
      (bracket [])

    , test "Bracketing a singleton should give that element three times" <|
      assertEqual
      [8, 8, 8]
      (bracket [8])

    , test "Bracketing [a,b] should give [a,a,b,b]" <|
      assertEqual
      [3, 3, 4, 4]
      (bracket [3, 4])

    , test "Bracketing [a,b,c] should give [a,a,b,c,c]" <|
      assertEqual
      [5,5,6,7,7]
      (bracket [5,6,7])

    ]

bracketMapTest : Test
bracketMapTest =
    suite "bracketMapTest"

    [ test "With empty list should yield empty list" <|
      assertEqual
      []
      (bracketMap identity identity [])

    , test "Simple test should work (1)" <|
      assertEqual
      ["ya", "ay", "bee", "sea", "seasea"]
      (bracketMap String.reverse (String.repeat 2) ["ay", "bee", "sea"])

    , test "Simple test should work (2)" <|
      assertEqual
      ["Xay", "ay", "bee", "sea", "aes"]
      (bracketMap (String.cons 'X') String.reverse ["ay", "bee", "sea"])

    ]

spliceOneTest : Test
spliceOneTest =
    suite "spliceOneTest"

    [ test "Splicing in at index 0 should be okay" <|
      assertEqual
      [66, 77, 88, 4, 3, 2, 1]
      (spliceOne 0 [66, 77, 88] [5, 4, 3, 2, 1])

    , test "Splicing in at last index should be okay" <|
      assertEqual
      [6, 5, 4, 99, 88]
      (spliceOne 3 [99, 88] [6, 5, 4, 7])

    , test "Splicing in at the middle should be okay" <|
      assertEqual
      [6, 5, 99, 88, 3]
      (spliceOne 2 [99, 88] [6, 5, 4, 3])

    , test "Splicing in nothing at the middle should just drop the element" <|
      assertEqual
      [6, 5, 3]
      (spliceOne 2 [] [6, 5, 4, 3])

    , test "Splicing at a negative index should leave list unchanged" <|
      assertEqual
      [6, 5, 4, 3]
      (spliceOne -1 [99, 88] [6, 5, 4, 3])

    , test "Splicing at an index too high should leave list unchanged" <|
      assertEqual
      [6, 5, 4, 3]
      (spliceOne 4 [99, 88] [6, 5, 4, 3])

    , test "Splicing into an empty list should yield original empty list" <|
      assertEqual
      []
      (spliceOne 0 [99, 88] [])

    ]
