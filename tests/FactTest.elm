module FactTest exposing (all)

import Fact exposing (..)
import Distribution exposing (Limit(AtMost, AtLeast))

import ElmTest exposing (..)

all : Test
all =
    suite "FactTest"
    [ initTest
    ]

initTest : Test
initTest =
    suite "initTest"

    [ test "init with all zeros and AtLeast should be okay" <|
      assertEqual
      { text = { probPerc = "0", limit = AtLeast, value = "0" }
      , data = { prob = 0, limit = AtLeast, value = 0 }
      }
      (init { prob = 0, limit = AtLeast, value = 0 })

    , test "init with all zeros and AtMost should be okay" <|
      assertEqual
      { text = { probPerc = "0", limit = AtMost, value = "0" }
      , data = { prob = 0, limit = AtMost, value = 0 }
      }
      (init { prob = 0, limit = AtMost, value = 0 })

    , test "init with other values should be okay" <|
      assertEqual
      { text = { probPerc = "20", limit = AtMost, value = "35" }
      , data = { prob = 0.20, limit = AtMost, value = 35 }
      }
      (init { prob = 0.20, limit = AtMost, value = 35 })

    ]
