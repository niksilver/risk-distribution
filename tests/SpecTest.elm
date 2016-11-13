module SpecTest exposing (all)

import Spec exposing (..)

import Zone exposing (Zone, inf)
import Segment exposing (Segment)
import Value exposing (Value(..))
import Block exposing (Rect, ChartBlock)

import ElmTest exposing (..)


all : Test
all =
    suite "SpecTest"
    [ fromSegmentsTest
    , rectsTest
    , scaleXTest
    , scaleYTest
    , transformXTest
    , transformYTest
    ]


-- Helper functions

removeSources : Spec -> Spec
removeSources spec =
    let
        updateValue v =
            case v of
                Exactly pc _ -> Exactly pc []
                Maximum pc _ -> Maximum pc []
                Contradiction _ -> Contradiction []
        updateChartBlock block =
            { block | value = updateValue block.value }

    in
        { spec | blocks = List.map updateChartBlock spec.blocks }


-- The tests...

fromSegmentsTest : Test
fromSegmentsTest =
    suite "fromSegmentsTest"

    [ test "For simple finite segments should give correct spec" <|
      let
        seg0 = Segment 100 (Zone 0 11)
        seg1 = Segment  50 (Zone 2 7)
        seg2 = Segment  80 (Zone 0 7)
        spec = fromSegments [seg0, seg1, seg2]
      in
        assertEqual
        ( Just
            { minX = 0
            , maxX = 11
            , maxY = 15
            , blocks =
                [ ChartBlock (Zone 0 2)  (Exactly 30 []) (Rect 0 2 15)
                , ChartBlock (Zone 2 7)  (Exactly 50 []) (Rect 2 7 10)
                , ChartBlock (Zone 7 11) (Exactly 20 []) (Rect 7 11 5)
                ]
            }
        )
        (spec |> Maybe.map removeSources)

    ]

rectsTest : Test
rectsTest =
    suite "rectsTest"

    [ test "Should return rects for simple spec" <|
      let
        spec =
          { minX = 0
          , maxX = 11
          , maxY = 15
          , blocks =
              [ ChartBlock (Zone 0 2)  (Exactly 30 []) (Rect 0 2 15)
              , ChartBlock (Zone 2 7)  (Exactly 50 []) (Rect 2 7 10)
              , ChartBlock (Zone 7 11) (Exactly 20 []) (Rect 7 11 5)
              ]
          }
      in
        assertEqual
        [ Rect 0 2 15
        , Rect 2 7 10
        , Rect 7 11 5
        ]
        (rects spec)
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
            , blocks =
                [ ChartBlock
                    (Zone 1 4)
                    (Exactly 8 [0, 1])
                    { left = 1, right = 4, height = 8 }
                , ChartBlock
                    (Zone 4 5)
                    (Exactly 10 [1])
                    { left = 4, right = 5, height = 10 }
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
            , blocks =
                [ ChartBlock
                    (Zone 1 4)
                    (Exactly 8 [1, 2])
                    { left = 1, right = 4, height = 8 }
                , ChartBlock
                    (Zone 4 5)
                    (Exactly 10 [2, 0])
                    { left = 4, right = 5, height = 10 }
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
            , blocks =
                [ ChartBlock
                    (Zone 1 4)
                    (Exactly 8 [1, 2])
                    { left = 1, right = 4, height = 8 }
                , ChartBlock
                    (Zone 4 5)
                    (Exactly 10 [2, 0])
                    { left = 4, right = 5, height = 10 }
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
            , blocks =
                [ ChartBlock
                    (Zone 1 4)
                    (Exactly 8 [1, 2])
                    { left = 1, right = 4, height = 8 }
                , ChartBlock
                    (Zone 4 5)
                    (Exactly 10 [2, 0])
                    { left = 4, right = 5, height = 10 }
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
