module PathTest exposing (all)

import Path exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "PathTest"
    [ dTest
    , mapTestForAbsoluteInstructions
    ]

dTest : Test
dTest =
    suite "dTest"

    [ test "d for empty Path should be empty string" <|
      assertEqual
      ""
      (Path [] |> d)

    , test "d for Path with single M should be right (1)" <|
      assertEqual
      "M 10,20"
      (Path [M 10 20] |> d)

    , test "d for Path with single M should be right (2)" <|
      assertEqual
      "M 12,13"
      (Path [M 12 13] |> d)

    , test "d for Path with single L should be right" <|
      assertEqual
      "L 20,21"
      (Path [L 20 21] |> d)

    , test "d for Path with mixed instrucions should be right" <|
      assertEqual
      "M 5,6 L 20,21"
      (Path [M 5 6, L 20 21] |> d)

    , test "d for Path with lower case m should be right" <|
      assertEqual
      "m 21,22"
      (Path [M' 21 22] |> d)

    , test "d for Path with lower case l should be right" <|
      assertEqual
      "l 20,21"
      (Path [L' 20 21] |> d)

    , test "d for Path with H should be right" <|
      assertEqual
      "H 66"
      (Path [H 66] |> d)

    , test "d for Path with lower case h should be right" <|
      assertEqual
      "h 67"
      (Path [H' 67] |> d)

    , test "d for Path with V should be right" <|
      assertEqual
      "V 71"
      (Path [V 71] |> d)

    , test "d for Path with lower case v should be right" <|
      assertEqual
      "v 72"
      (Path [V' 72] |> d)

    ]

simpleXYFn1 : Float -> Float -> (Float, Float)
simpleXYFn1 x y =
    (x*2 + 1, y*3 + 2)

simpleXYFn2 : Float -> Float -> (Float, Float)
simpleXYFn2 x y =
    (x*3 + 2, y*2 + 1)

mapTestForAbsoluteInstructions : Test
mapTestForAbsoluteInstructions =
    suite "mapTestForAbsoluteInstructions"

    [ test "Simple map on M should map values" <|
      assertEqual
      (Path [M 17 29])
      (Path [M 8 9] |> map simpleXYFn1)

    , test "Simple map on L should map values" <|
      assertEqual
      (Path [L 11 9])
      (Path [L 3 4] |> map simpleXYFn2)

    , test "Simple map on H should change the horizontal" <|
      assertEqual
      (Path [M 3 8, H 11])
      (Path [M 1 2, H 5] |> map simpleXYFn1)

    , test "Simple map on V should change the vertical" <|
      assertEqual
      (Path [M 11 7, V 9])
      (Path [M 3 3, V 4] |> map simpleXYFn2)

    ]

