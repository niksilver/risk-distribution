module ExpandTest exposing (all)

import Expand exposing (..)

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
        ([4, 5, 3, 12, 48, 60, 36] ,Nothing)
        (expand [4, 5] [3]
            { skip = skip
            , stop = stop
            , grow = grow
            , update = update
            }
        )
        -- (filteredExpand fn pred [4, 5] 3)
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
