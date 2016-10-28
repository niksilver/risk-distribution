module UtilTest exposing (all)

import Util exposing (..)

import String
import Dict

import ElmTest exposing (..)

all : Test
all =
    suite "UtilTest"
    [ isFiniteTest
    , singletonTest
    , noneTest
    , findTest
    , groupByTest
    , slidingTest
    , bracketTest
    , bracketMapTest
    , spliceOneTest
    , insertTest
    , ithTest
    , indexOfTest
    , dedupeTest
    , filteredExpandTest
    , toLetterTest
    ]

isFiniteTest : Test
isFiniteTest =
    suite "isFiniteTest"

    [ test "-Infinity should not be finite" <|
      assertEqual
      False
      (isFinite (-1/0))

    , test "+Infinity should not be finite" <|
      assertEqual
      False
      (isFinite (1/0))

    , test "0/0 should not be finite" <|
      assertEqual
      False
      (isFinite (0/0))

    , test "sqrt(-1) should not be finite" <|
      assertEqual
      False
      (isFinite (sqrt -1))

    , test "-0.5 should be finite" <|
      assertEqual
      True
      (isFinite -0.5)

    , test "1.5 should be finite" <|
      assertEqual
      True
      (isFinite 1.5)

    ]

singletonTest : Test
singletonTest =
    suite "singletonTest"

    [ test "Simple singleton should work (1)" <|
      assertEqual [33] (singleton 33)

    , test "Simple singleton should work (2)" <|
      assertEqual ["hello"] (singleton "hello")
    ]

noneTest : Test
noneTest =
    suite "noneTest"

    [ test "None isEven in [3, 4, 7] should be False" <|
      let
        isEven a = (a % 2 == 0)
      in
        assertEqual
        False
        (none isEven [3, 4, 7])

    , test "None isEven in [7, 9, 1] should be True" <|
      let
        isEven a = (a % 2 == 0)
      in
        assertEqual
        True
        (none isEven [7, 9, 1])

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

groupByTest : Test
groupByTest =
    suite "groupByTest"

    [ test "Grouping empty list should yield empty Dict" <|
      assertEqual
      Dict.empty
      (groupBy (identity) [])

    , test "Grouping singleton list should yield singleton Dict" <|
      assertEqual
      (Dict.singleton "Hi" ["Hi"])
      (groupBy (identity) ["Hi"])

    , test "Grouping distinct values by identity should yield individual values" <|
      assertEqual
      [(2, [2]), (3, [3]), (5, [5]), (6, [6])]
      (groupBy (identity) [6, 5, 2, 3] |> Dict.toList)

    , test "Grouping distinct values by 'constant discriminator' should yield all values together" <|
      assertEqual
      [("x", [6, 5, 2, 3])]
      (groupBy (always "x") [6, 5, 2, 3] |> Dict.toList)

    , test "Grouping by non-simple function should work" <|
      assertEqual
      [(4, [54, 14]), (6, [6])]
      (groupBy (\a -> a % 10) [54, 6, 14] |> Dict.toList)

    , test "Grouping duplicates with a non-simple function should reproduce the duplicates" <|
      assertEqual
      [(4, [54, 54]), (6, [6])]
      (groupBy (\a -> a % 10) [54, 6, 54] |> Dict.toList)

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

insertTest : Test
insertTest =
    suite "insertTest"

    [ test "Inserting at the start should work" <|
      assertEqual
      [10, 20, 88, 99]
      (insert 0 [10, 20] [88, 99])

    , test "Inserting at the end should work" <|
      assertEqual
      [88, 99, 10, 20]
      (insert 2 [10, 20] [88, 99])

    , test "Inserting in the middle should work" <|
      assertEqual
      [77, 10, 11, 88, 99]
      (insert 1 [10, 11] [77, 88, 99])

    , test "Inserting beyond the end should have no effect" <|
      assertEqual
      [77, 88, 99]
      (insert 4 [10, 11] [77, 88, 99])

    , test "Inserting at a negative index should have no effect" <|
      assertEqual
      [66, 77, 88]
      (insert -1 [10, 11] [66, 77, 88])

    , test "Inserting at 0 index for an empty list should work" <|
      assertEqual
      [10, 20]
      (insert 0 [10, 20] [])

    ]

ithTest : Test
ithTest =
    suite "ithTest"

    [ test "0th of a list is first item" <|
      assertEqual
      (Just 44)
      (ith 0 [44, 55, 66])

    , test "2th of a list is third item" <|
      assertEqual
      (Just 66)
      (ith 2 [44, 55, 66])

    , test "When i too large on non-empty list we get nothing" <|
      assertEqual
      (Nothing)
      (ith 3 [44, 55, 66])

    , test "When i = 0 on empty list we get nothing" <|
      assertEqual
      (Nothing)
      (ith 0 [])

    , test "When i < 0 on non-empty list we get nothing" <|
      assertEqual
      (Nothing)
      (ith -1 [44, 55, 66])

    ]

indexOfTest : Test
indexOfTest =
    suite "indexOfTest"

    [ test "First element should be found" <|
      assertEqual
      (Just 0)
      (indexOf 'a' ['a', 'b', 'c'])

    , test "Second element should be found" <|
      assertEqual
      (Just 1)
      (indexOf 'b' ['a', 'b', 'c'])

    , test "Third element should be found" <|
      assertEqual
      (Just 2)
      (indexOf 'c' ['a', 'b', 'c'])

    , test "Unknown element should not be found" <|
      assertEqual
      (Nothing)
      (indexOf 'x' ['a', 'b', 'c'])

    ]

dedupeTest : Test
dedupeTest =
    suite "dedupeTest"

    [ test "Deduping empty list should give empty list" <|
      assertEqual
      []
      (dedupe (==) [])

    , test "Deduping by equality should simply remove duplicates" <|
      assertEqual
      [4, 2, 6, 1]
      (dedupe (==) [4, 2, 6, 4, 6, 1, 2])

    , test "Deduping by last digit should work" <|
      assertEqual
      [14, 12, 6, 1]
      (dedupe (\a b -> a % 10 == b % 10) [14, 12, 6, 24, 36, 1, 92])

    ]

filteredExpandTest : Test
filteredExpandTest =
    suite "filteredExpandTest"

    [ test "Expand on multiplication, and filter on unique last digit" <|
      let
        fn prev a = List.map (\p -> p * a) prev
        pred xs a =
            List.any (\x -> x % 10 == a % 10) xs |> not
      in
        assertEqual
        [4, 5, 3, 12, 48, 60, 36]
        (filteredExpand fn pred [4, 5] 3)

     , test "Expand with addition, and filter on unique last digit" <|
      let
        fn prev a = List.map (\p -> p + a) prev
        pred xs a =
            Util.none (\x -> x % 10 == a % 10) xs
      in
        assertEqual
        [1, 2, 4, 5, 6, 7, 9, 8, 10, 13]
        (filteredExpand fn pred [1, 2] 4)

    , test "Expand by summing, and filter so modulo 10 an elt is unique and between the highest and lowest" <|
      let
        fn prev a = List.map (\p -> p + a) prev
        pred xs elt =
            let
                elt10 = elt % 10
                digits = List.map (\x -> x % 10) xs
                isUnique = not (List.member elt10 digits)
                min = List.minimum digits
                max = List.maximum digits
                inRange y lower upper =
                    (lower < y) && (y < upper)
                inMaxMinRange y =
                    (Maybe.map2 (inRange y) min max) == Just True
            in
                isUnique &&
                    inMaxMinRange (elt10)
      in
        assertEqual
        [1, 7, 4, 5, 6, 12, 13]
        (filteredExpand fn pred [1, 7] 4)
        -- [1, 7] [4]
        --     Filter: [4]
        --     Expand: [5, 11]
        -- [1, 7, 4] [5, 11]
        --     Filter: [5]
        --     Expand: [6, 12, 9]
        -- [1, 7, 4, 5] [6, 12, 9]
        --     Filter: [6, 12]
        --     Expand: [7, 13, 10, 11]
        -- [1, 7, 4, 5, 6] [12, 7, 13, 10, 11]
        --     Filter: [12, 13]
        --     Expand: [13, 19, 16, 17, 18]
        -- [1, 7, 4, 5, 6, 12] [13, 13, 19, 16, 17, 18]
        --     Filter: [13, 13]
        --     Expand: [14, 20, 17, 18, 19, 25]
        -- [1, 7, 4, 5, 6, 12, 13] [13, 14, 20, 17, 18, 19, 25]
        --     Filter: []
        --     Expand: []
        -- which gives us our result

    , test "Expanding with new values should give nothing if predicate always false" <|
      let
        fn prev a = [4, 5, 6]
        pred xs elt = False
      in
        assertEqual
        [1, 2, 3]
        (filteredExpand fn pred [1, 2, 3] 88)

    , test "Should be able to expand to a max list length" <|
      let
        fn prev a = [4, 5]
        pred xs elt = (List.length xs <= 8)
      in
        assertEqual
        [1, 2, 3, 88, 4, 5, 4, 5, 4]  -- 8 elements, plus the final seed makes 9
        (filteredExpand fn pred [1, 2, 3] 88)
    ]

toLetterTest : Test
toLetterTest =
    suite "toLetterTest"

    [ test "0 should give a" <|
      assertEqual "a" (toLetter 0)

    , test "1 should give b" <|
      assertEqual "b" (toLetter 1)

    , test "25 should give z" <|
      assertEqual "z" (toLetter 25)

    , test "-1 should give ?" <|
      assertEqual "?" (toLetter -1)

    , test "26 should give ?" <|
      assertEqual "?" (toLetter 26)

    ]
