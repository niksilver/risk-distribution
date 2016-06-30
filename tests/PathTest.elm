module PathTest exposing (all)

import Path exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "PathTest"
    [ dTest
    , mapXTest
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

mapXTest : Test
mapXTest =
    suite "mapXTest"

    [ test "Simple map x for M should map only x" <|
      assertEqual
      (Path [M 34 44])
      (Path [M 33 44] |> mapX (\x -> x + 1))

    , test "Simple map x for m should map only x" <|
      assertEqual
      (Path [M' 35 44])
      (Path [M' 33 44] |> mapX (\x -> x + 2))

    , test "Simple map x for L should map only x" <|
      assertEqual
      (Path [L 36 44])
      (Path [L 33 44] |> mapX (\x -> x + 3))

    , test "Simple map x for l should map only x" <|
      assertEqual
      (Path [L' 37 44])
      (Path [L' 33 44] |> mapX (\x -> x + 4))

    , test "Simple map x for H should map x" <|
      assertEqual
      (Path [H 45])
      (Path [H 44] |> mapX (\x -> x + 1))

    , test "Simple map x for h should map x" <|
      assertEqual
      (Path [H' 46])
      (Path [H' 44] |> mapX (\x -> x + 2))

    , test "Simple map x for V should not map argument" <|
      assertEqual
      (Path [V 55])
      (Path [V 55] |> mapX (\x -> x + 1))

    , test "Simple map x for v should not map argument" <|
      assertEqual
      (Path [V' 55])
      (Path [V' 55] |> mapX (\x -> x + 2))

    ]

