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
    , taperComparatorTest
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

taperComparatorTest : Test
taperComparatorTest =
    let
        -- When we list all these blocks they don't have to be contiguous
        -- for the sake of the function, even though we expect them to
        -- be in actual use

        b0 = Block (Zone -inf -4) (Exactly 25 [2])   -- From -inf
        b1 = Block (Zone -4 0) (Exactly 10 [1])
        b2 = Block (Zone 0 4) (Exactly 5 [1, 0])
        b3 = Block (Zone 4 6) (Maximum 0 [2, 1, 0])  -- Zero height
        b4 = Block (Zone 6 10) (Exactly 0 [0, 1])    -- Zero height
        b5 = Block (Zone 10 20) (Maximum 85 [2, 3])
        b6 = Block (Zone 20 inf) (Maximum 15 [3, 0])  -- To inf
    in
        suite "taperComparatorTest"

        [ test "Comparator for a left taper should be next one if it's finite" <|
          assertEqual
          b1
          (taperComparator b0 [b0, b1, b2, b3])

        , test "Comparator for a left taper should be first finite one if next to a zero block" <|
          assertEqual
          b5
          (taperComparator b0 [b0, b3, b4, b5])

        , test "Comparator for a left taper should be sensible if it's followed by zeros and a right-taper" <|
          assertEqual
          { zone = Zone 0 1, value = Exactly 1 [] }
          (taperComparator b0 [b0, b3, b4, b6])

        , test "Comparator for a left taper should be sensible if there's nothing else" <|
          assertEqual
          { zone = Zone 0 1, value = Exactly 1 [] }
          (taperComparator b0 [b0])

        , test "Comparator for a right taper should be previous one if it's finite" <|
          assertEqual
          b5
          (taperComparator b5 [b2, b5, b6])

        , test "Comparator for a right taper should be last finite one if next to a zero block" <|
          assertEqual
          b2
          (taperComparator b6 [b1, b2, b3, b4, b6])

        , test "Comparator for a right taper should be sensible if it's preceeded by zeros and a right-taper" <|
          assertEqual
          { zone = Zone 0 1, value = Exactly 1 [] }
          (taperComparator b6 [b0, b3, b4, b6])

        , test "Comparator for a right taper should be sensible if there's nothing else" <|
          assertEqual
          { zone = Zone 0 1, value = Exactly 1 [] }
          (taperComparator b6 [b6])

        ]

toChartBlockTest : Test
toChartBlockTest =
    let
        -- When we list all these blocks they don't have to be contiguous
        -- for the sake of the function, even though we expect them to
        -- be in actual use

        b0 = Block (Zone -inf -4) (Exactly 25 [2])   -- From -inf
        b1 = Block (Zone -4 0) (Exactly 10 [1])
        b2 = Block (Zone 0 4) (Exactly 5 [1, 0])
        b3 = Block (Zone 4 6) (Maximum 0 [2, 1, 0])  -- Zero height
        b4 = Block (Zone 6 10) (Exactly 0 [0, 1])     -- Zero height
        b5 = Block (Zone 10 20) (Maximum 85 [2, 3])
    in
        suite "toChartBlockTest"

        [ test "ChartBlock for exact finite %age should have rect height spread over width" <|
          assertEqual
          [ChartBlock (Zone 0 4) (Exactly 5 [1, 0]) (Rect 0 4 (5/4))]
          (toChartBlock b2 [b0, b1, b2])

        , test "ChartBlock for maximum finite %age should have rect heigh spread over" <|
          assertEqual
          [ChartBlock (Zone 10 20) (Maximum 85 [2, 3]) (Rect 10 20 (85/10))]
          (toChartBlock b5 [b0, b1, b2, b5])

        , suite "For Block which runs from -inf and is next to a non-zero finite block"
          -- We're looking at a block of size 25% tapering off to -inf
          -- next to a block of 10% size and 4 wide (from -4 to 0)
          -- (therefore its rect height is 10 / 4 = 2.5).
          -- So for the tapering chart block its first rect (on the right)
          -- should be half the 2.5 high, i.e. 1.25 high,
          -- and its width should be 10 wide (because 10 * 1.25 is half
          -- the total area of its 25% size).
          -- Therefore its second rect should be 0.625 high (half again) and 20 wide
          -- and its third block should be 0.3125 high and 20 wide, etc
          -- for five blocks (because that's our taperFactor).

          [ test "Last ChartBlock should be appropriate dimensions" <|
            assertEqual
            (Just
                { zone = Zone -inf -4
                , value = Exactly 25 [2]
                , rect =
                    { left = -4 - 10
                    , right = -4
                    , height = 2.5 / 2
                    }
                })
            (toChartBlock b0 [ b0, b1, b2 ]
                |> List.reverse
                |> Util.ith 0
            )

          , test "Second last ChartBlock should be appropriate dimensions" <|
            assertEqual
            (Just
                { zone = Zone -inf -4
                , value = Exactly 25 [2]
                , rect =
                    { left = -4 - 2*10
                    , right = -4 - 10
                    , height = 2.5 / 4
                    }
                })
            (toChartBlock b0 [ b0, b1, b2 ]
                |> List.reverse
                |> Util.ith 1
            )

          , test "Third last ChartBlock should be appropriate dimensions" <|
            assertEqual
            (Just
                { zone = Zone -inf -4
                , value = Exactly 25 [2]
                , rect =
                    { left = -4 - 3*10
                    , right = -4 - 2*10
                    , height = 2.5 / 8
                    }
                })
            (toChartBlock b0 [ b0, b1, b2 ]
                |> List.reverse
                |> Util.ith 2
            )

          , test "There should be taperFactor ChartBlock elements" <|
            assertEqual
            taperFactor
            (toChartBlock b0 [ b0, b1, b2 ]
                |> List.length
            )

          ]

        ]
