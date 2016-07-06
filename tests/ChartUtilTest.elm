module ChartUtilTest exposing (all)

import ChartUtil exposing (..)

import ElmTest exposing (..)


all : Test
all =
    suite "ChartUtilTest"
    [ scaleXTest
    , scaleYTest
    , transformXTest
    , transformYTest
    , mergeSimilarTest
    ]


scaleXTest : Test
scaleXTest =
    let
        viewDims =
            { left = 3
            , right = 3
            , top = 8
            , bottom = 8
            , width = 1000
            , height = 50
            }
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
        viewDims =
            { left = 3
            , right = 3
            , top = 8
            , bottom = 8
            , width = 1000
            , height = 50
            }
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
        viewDims =
            { left = 3
            , right = 3
            , top = 8
            , bottom = 8
            , width = 1000
            , height = 50
            }
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
        viewDims =
            { left = 3
            , right = 3
            , top = 8
            , bottom = 8
            , width = 1000
            , height = 50
            }
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

mergeSimilarTest : Test
mergeSimilarTest =
    let
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
    suite "mergeSimilarTest"

    [ test "Very different rects should remain unmerged" <|
      let
          spec =
              { minX = 1
              , maxX = 7
              , maxY = 10
              , rects =
                  [ { left = 1, right = 4, height = 8 }
                  , { left = 4, right = 5, height = 10 }
                  , { left = 5, right = 7, height = 3 }
                  ]
              }
       in
           assertEqual
           spec
           (mergeSimilar 0.1 spec)

    , test "Two similar rects in the middle should merge" <|
      let
          spec =
              { minX = 1
              , maxX = 8
              , maxY = 10
              , rects =
                  [ { left = 1, right = 4, height = 3 }
                  , { left = 4, right = 5, height = 10 }
                  , { left = 5, right = 7, height = 9 }
                  , { left = 7, right = 8, height = 6 }
                  ]
              }
       in
           assertEqual
           { spec
           | rects =
               [ { left = 1, right = 4, height = 3 }
               , { left = 4, right = 7, height = 9.5 }
               , { left = 7, right = 8, height = 6 }
               ]
           }
           (mergeSimilar 0.1 spec)

    ]
