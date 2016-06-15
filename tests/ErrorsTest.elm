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
    ]

