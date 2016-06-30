module PathTest exposing (all)

import Path exposing (..)

import ElmTest exposing (..)

all : Test
all =
    suite "PathTest"
    [ dTest
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

    , test "d for Path with lower case l should be right" <|
      assertEqual
      "l 20,21"
      (Path [L' 20 21] |> d)

    ]

