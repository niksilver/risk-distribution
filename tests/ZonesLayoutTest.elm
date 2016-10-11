module ZonesLayoutTest exposing (all)

import ZonesLayout exposing (..)

import Zone exposing (inf, Zone)
import ZoneDict exposing (Value (Exactly, Maximum, Contradiction))
import ChartUtil exposing (Rect)
import Util

import ElmTest exposing (..)

all : Test
all =
    suite "ZoneLayoutTest"
    [ trimTest
    , taperZoneWidthTest
    , toChartBlockTest
    , toRangeTest
    ]

trimTest : Test
trimTest =
    suite "trimTest"

    [ test "Non-zero blocks should not be trimmed" <|
      let
        blocks =
            [ Block (Zone 0 1) (Maximum 10 [0, 1])
            , Block (Zone 1 4) (Exactly 85 [2])
            , Block (Zone 4 5) (Maximum 5 [1, 2])
            ]
      in
        assertEqual
        blocks
        (trim blocks)

    , test "Zero-blocks at the start should be trimmed" <|
      let
        block1 = Block (Zone 0 1) (Maximum 0 [0, 1])
        block2 = Block (Zone 1 3) (Exactly 0 [2, 1])
        block3 = Block (Zone 3 10) (Maximum 50 [0])
        block4 = Block (Zone 10 inf) (Maximum 50 [0, 2])
      in
        assertEqual
        [ block3, block4 ]
        (trim [ block1, block2, block3, block4 ])

    , test "Zero-blocks at the end should be trimmed" <|
      let
        block1 = Block (Zone 0 1) (Maximum 20 [0, 1])
        block2 = Block (Zone 1 3) (Exactly 80 [2, 1])
        block3 = Block (Zone 3 10) (Maximum 0 [0])
        block4 = Block (Zone 10 inf) (Maximum 0 [0, 2])
      in
        assertEqual
        [ block1, block2 ]
        (trim [ block1, block2, block3, block4 ])

    , test "Zero-blocks in the middle should not be trimmed" <|
      let
        block1 = Block (Zone 0 1) (Maximum 20 [0, 1])
        block2 = Block (Zone 1 3) (Exactly 0 [2, 1])
        block3 = Block (Zone 3 10) (Maximum 0 [0])
        block4 = Block (Zone 10 inf) (Maximum 80 [0, 2])
      in
        assertEqual
        [ block1, block2, block3, block4 ]
        (trim [ block1, block2, block3, block4 ])

    ]

taperZoneWidthTest : Test
taperZoneWidthTest =
    suite "taperZoneWidthTest"

    [ test "Equal %ages should give width which is a constant bigger (1)" <|
      assertEqual
      (4)
      (taperZoneWidth 50 4 50)

    , test "Equal %ages should give width which is a constant bigger (2)" <|
      assertEqual
      (10)
      (taperZoneWidth 20 10 20)

    , test "Zone of half the prob should give width which is half a constant bigger (1)" <|
      assertEqual
      (6 / 2)
      (taperZoneWidth 15 6 30)

    , test "Zone of half the prob should give width which is half a constant bigger (2)" <|
      assertEqual
      (10 / 2)
      (taperZoneWidth 20 10 40)

    , test "Zone of three times the prob should give width which is 3 x constant bigger (1)" <|
      assertEqual
      (50 * 3)
      (taperZoneWidth 15 50 5)

    , test "Zone of three times the prob should give width which is 3 x constant bigger (2)" <|
      assertEqual
      (4 * 3)
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

toChartBlockTest : Test
toChartBlockTest =
    let
        -- When we list all these blocks they don't have to be contiguous
        -- for the sake of the function, even though we expect them to
        -- be in actual use

        b0 = Block (Zone -inf -4) (Exactly 25 [2])
        b1 = Block (Zone -4 0) (Exactly 10 [1])
        b2 = Block (Zone 0 1) (Exactly 5 [1, 0])
        b3 = Block (Zone 1 2) (Maximum 0 [2, 1, 0])
        b4 = Block (Zone 2 5) (Exactly 0 [0, 1])
        b5 = Block (Zone 5 10) (Maximum 85 [2, 3])
    in
        suite "toChartBlockTest"

        [ test "ChartBlock for exact finite %age should have rect same as block" <|
          assertEqual
          [ChartBlock (Zone 0 1) (Exactly 5 [1, 0]) (Rect 0 1 5)]
          (toChartBlock b2 [b0, b1, b2])

        , test "ChartBlock for maximum finite %age should have rect same as block" <|
          assertEqual
          [ChartBlock (Zone 5 10) (Maximum 85 [2, 3]) (Rect 5 10 85)]
          (toChartBlock b5 [b0, b1, b2, b5])

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
