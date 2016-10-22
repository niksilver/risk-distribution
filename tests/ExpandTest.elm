module ExpandTest exposing (all)

import Expand exposing (..)

import String

import ElmTest exposing (..)


all : Test
all =
    suite "ExpandTest"
    [ expandTest
    , skipWhileTest
    ]

expandTest : Test
expandTest =
    suite "expandTest"

    [ test "Expansion should stop when the queue is empty" <|
      let
        -- Skip the head of the queue if the last digit is already in the data list
        skip xs h = List.any (\x -> x % 10 == h % 10) xs
        -- Never stop using the stop check
        stop xs h = False
        -- Grow the queue by multiplying each elt of the data with the queue head
        grow xs h = List.map (\x -> x * h) xs
        -- Update just by putting the head of the queue at the end of the data
        update xs h = List.append xs [h]
        -- fn prev a = List.map (\p -> p * a) prev
        -- pred xs a =
        --     List.any (\x -> x % 10 == a % 10) xs |> not
      in
        assertEqual
        ([4, 5, 3, 12, 48, 60, 36], Nothing)
        (expand [4, 5] [3]
            { skip = skip
            , stop = stop
            , grow = grow
            , update = update
            }
        )

    , test "Expansion should stop when we get to a problematic queue entry" <|
      let
        -- Skip the head of the queue if the last digit is already in the data list
        skip xs h = List.any (\x -> x % 10 == h % 10) xs
        -- Stop if the head of the queue is 10 or more
        stop xs h = h >= 10
        -- Grow the queue by adding each elt of the data with the queue head
        grow xs h = List.map (\x -> x + h) xs
        -- Update just by putting the head of the queue at the end of the data
        update xs h = List.append xs [h]
      in
        assertEqual
        ([1, 7, 4, 5, 6], Just 12)
        (expand [1, 7] [4]
            { skip = skip
            , stop = stop
            , grow = grow
            , update = update
            }
        )
        -- [1, 7] [4]
        --     No skipping, and don't stop on the 4
        --     Grow with: [5, 11]
        -- [1, 7, 4] [5, 11]
        --     No skipping, and don't stop on the 5
        --     Grow with: [6, 12, 9]
        -- [1, 7, 4, 5] [11, 6, 12, 9]
        --     Skip 11 only, leaving [6, 12, 9]
        --     Use the 6 to grow with: [7, 13, 10, 11]
        -- [1, 7, 4, 5, 6] [12, 9, 7, 13, 10, 11]
        --     No skipping, but stop because 12 is too big
        -- which gives us our result

    , test "Expansion should work when the data and queue are of different types" <|
      let
        -- Never skip the head of the queue
        skip xs h = False
        -- Stop if the data is 10 characters or more
        stop xs h = String.length xs >= 10
        -- Grow the queue by adding on the length of the data string squared
        grow xs h = [ String.length xs ^ 2 ]
        -- Update just by appending the queue head to the end of the data string
        update xs h = xs ++ toString h
      in
        assertEqual
        ("a0149162549", Just 81)
        (expand "a" [0]
            { skip = skip
            , stop = stop
            , grow = grow
            , update = update
            }
        )


    ]

skipWhileTest : Test
skipWhileTest =
    suite "skipWhileTest"

    [ test "Skipping a few should work" <|
      assertEqual
      [5, 4, 6]
      (skipWhile
        (\x -> x < 4)
        [0, -1, 3, 5, 4, 6]
      )

    , test "Skipping none should give the original list" <|
      assertEqual
      [9, 10, 11, 12]
      (skipWhile
        (always False)
        [9, 10, 11, 12]
      )

    , test "Skipping all should give the empty list" <|
      assertEqual
      []
      (skipWhile
        (always True)
        [9, 10, 11, 12]
      )

    ]
