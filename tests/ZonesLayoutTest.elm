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
