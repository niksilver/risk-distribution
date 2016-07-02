module PathTest exposing (all)

import Path exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "PathTest"
    [ dTest
    , mapTestForAbsoluteInstructions
    , mapTestForRelativeInstructions
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

mapTestForAbsoluteInstructions : Test
mapTestForAbsoluteInstructions =
    let
        simpleXYFn1 : Float -> Float -> (Float, Float)
        simpleXYFn1 x y =
            (x*x + 1, y + x)

        simpleXYFn2 : Float -> Float -> (Float, Float)
        simpleXYFn2 x y =
            (y, x)
    in
        suite "mapTestForAbsoluteInstructions"

        [ test "Simple map on M should map values" <|
          assertEqual
          (Path [M 5 6])
          (Path [M 2 4] |> map simpleXYFn1)

        , test "Simple map on L should map values" <|
          assertEqual
          (Path [L 4 3])
          (Path [L 3 4] |> map simpleXYFn2)

        , test "Simple map on H should generate a new line" <|
          assertEqual
          (Path [M 2 3, L 26 7])
          (Path [M 1 2, H 5] |> map simpleXYFn1)

        , test "Simple map on V should generate a new line" <|
          assertEqual
          (Path [M 2 3, L 4 3])
          (Path [M 3 2, V 4] |> map simpleXYFn2)

        ]

mapTestForRelativeInstructions : Test
mapTestForRelativeInstructions =
    let
        simpleXYFn1 : Float -> Float -> (Float, Float)
        simpleXYFn1 x y =
            (x*x + 1, y + x)

        simpleXYFn2 : Float -> Float -> (Float, Float)
        simpleXYFn2 x y =
            (y, x)
    in
        suite "mapTestForRelativeInstructions"

        [ test "Simple map on m should map its values" <|
          assertEqual
          (Path [M 5 6, M 17 11])
          -- This path goes (2, 4) -> (4, 7)
          (Path [M 2 4, M' 2 3] |> map simpleXYFn1)

        , test "Simple map on l should map values" <|
          assertEqual
          (Path [M 4 3, L 7 4])
          -- This path goes (3, 4) -> (4, 7)
          (Path [M 3 4, L' 1 3] |> map simpleXYFn2)

        , test "Simple map on h should change the horizontal" <|
          assertEqual
          (Path [M 2 3, L 37 8])
          -- This path goes (1, 2) -> (6, 2)
          (Path [M 1 2, H' 5] |> map simpleXYFn1)

        , test "Simple map on b should change the vertical" <|
          assertEqual
          (Path [M 2 3, L 6 3])
          -- This path goes (3, 2) -> (3, 6)
          (Path [M 3 2, V' 4] |> map simpleXYFn2)

        ]

