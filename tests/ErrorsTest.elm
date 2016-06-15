module ErrorsTest exposing (all)

import Errors exposing (..)
import Distribution exposing (Limit(AtMost, AtLeast))

import ElmTest exposing (..)

all : Test
all =
    suite "ErrorsTest"
    [ errorsTest
    ]

errorsTest : Test
errorsTest =
    suite "errorsTest"

    [ test "Simple bounds should give no errors" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 20.0 }
          y2 = { prob = 1.00, limit = AtMost, value = 50.0 }
      in
          assertEqual
          []
          (errors [y1, y2])

    , test "Simple bounds plus okay layer should give no errors" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 20.0 }
          y2 = { prob = 0.50, limit = AtLeast, value = 30.0 }
          y3 = { prob = 1.00, limit = AtMost, value = 50.0 }
      in
          assertEqual
          []
          (errors [y1, y2, y3])

    , test "Layers facing away from each other with > 100% should report error" <|
      let
          y1 = { prob = 0.51, limit = AtMost, value = 20.0 }
          y2 = { prob = 0.60, limit = AtLeast, value = 50.0 }
      in
          assertEqual
          [ MoreThan100Percent ]
          (errors [y1, y2])

    , test "Layers facing away from each other reversed with > 100% should report error" <|
      let
          y1 = { prob = 0.60, limit = AtLeast, value = 50.0 }
          y2 = { prob = 0.51, limit = AtMost, value = 20.0 }
      in
          assertEqual
          [ MoreThan100Percent ]
          (errors [y1, y2])

    , test "Layers facing away from each other with > 100% and hidden among others should report error" <|
      let
          y1 = { prob = 0.51, limit = AtMost, value = 20.0 }
          y2 = { prob = 0.60, limit = AtLeast, value = 10.0 }
          y3 = { prob = 0.55, limit = AtLeast, value = 50.0 }
      in
          assertEqual
          [ MoreThan100Percent ]
          (errors [y1, y2, y3])

    ]

