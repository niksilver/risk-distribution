module UtilTest exposing (all)

import Util exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "UtilTest"
    [ findTest
    , findPairTest
    , scaleXTest
    , scaleYTest
    , transformXTest
    , transformYTest
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

scaleXTest : Test
scaleXTest =
    let
        viewDims = { left = 3, top = 8, width = 1000, height = 50 }
        spec =
            { minX = 1
            , maxX = 5
            , maxY = 10
            , rects =
                [ { left = 1, right = 4, height = 8 }
                , { left = 4, right = 5, height = 10 }
                ]
            }
    in
    suite "scaleXTest"

    [ test "Scale x measure for zero length" <|
      assertEqual
      0
      (scaleX viewDims spec 0)

    , test "Scale x measure for full length" <|
      assertEqual
      1000
      (scaleX viewDims spec 4)

    , test "Scale x measure for middle length" <|
      assertEqual
      250
      (scaleX viewDims spec 1)

    ]

scaleYTest : Test
scaleYTest =
    let
        viewDims = { left = 3, top = 8, width = 1000, height = 50 }
        spec =
            { minX = 1
            , maxX = 5
            , maxY = 10
            , rects =
                [ { left = 1, right = 4, height = 8 }
                , { left = 4, right = 5, height = 10 }
                ]
            }
    in
    suite "scaleYTest"

    [ test "Scale y measure for zero height" <|
      assertEqual
      0
      (scaleY viewDims spec 0)

    , test "Scale y measure for full height" <|
      assertEqual
      50
      (scaleY viewDims spec 10)

    , test "Scale y measure for middle height" <|
      assertEqual
      10
      (scaleY viewDims spec 2)

    ]


transformXTest : Test
transformXTest =
    let
        viewDims = { left = 3, top = 8, width = 1000, height = 50 }
        spec =
            { minX = 1
            , maxX = 5
            , maxY = 10
            , rects =
                [ { left = 1, right = 4, height = 8 }
                , { left = 4, right = 5, height = 10 }
                ]
            }
    in
    suite "transformXTest"

    [ test "Transform x test for LHS" <|
      assertEqual
      3
      (transformX viewDims spec 1)

    , test "Transform x test for RHS" <|
      assertEqual
      1003
      (transformX viewDims spec 5)

    , test "Transform x test for middle" <|
      assertEqual
      (250 + 3)
      (transformX viewDims spec 2)

    ]

transformYTest : Test
transformYTest =
    let
        viewDims = { left = 3, top = 8, width = 1000, height = 50 }
        spec =
            { minX = 1
            , maxX = 5
            , maxY = 10
            , rects =
                [ { left = 1, right = 4, height = 8 }
                , { left = 4, right = 5, height = 10 }
                ]
            }
    in
    suite "transformYTest"

    [ test "Transform y test for top" <|
      assertEqual
      8
      (transformY viewDims spec 10)

    , test "Transform y test for bottom" <|
      assertEqual
      58
      (transformY viewDims spec 0)

    , test "Transform y test for middle" <|
      assertEqual
      (8 + 40)
      (transformY viewDims spec 2)

    ]

