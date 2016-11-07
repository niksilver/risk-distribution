module FactListTest exposing (all)

import FactList exposing (..)

import Fact exposing (Limit (AtLeast, AtMost, Between))
import Segment exposing (Segment)
import Zone exposing (Zone, inf)

import ElmTest exposing (..)


all : Test
all =
    suite "FactListTest"
    [ changedTest
    ]

changedTest : Test
changedTest =
    suite "changedTest"

    [ test "If all facts' text match their data then nothing is being changed" <|
      assertEqual
      (Nothing)
      ( { next = 2
        , iFacts =
            [ { id = 0
              , fact = Fact.init (Segment 100 (Zone 0 10))
              }
            , { id = 1
              , fact = Fact.init (Segment 50 (Zone 2 8))
              }
            ]
        }

        |> changed
      )

    , test "If the last fact's text doesn't match its data then it is being changed" <|
      assertEqual
      (Just 23)
      ( { next = 2
        , iFacts =
            [ { id = 0
              , fact = Fact.init (Segment 100 (Zone 0 10))
              }
            , { id = 16
              , fact = Fact.init (Segment 50 (Zone 2 8))
              }
            , { id = 23
                , fact =
                    { text = { probPerc = "40", limit = AtLeast, lower = "5", upper = "" }
                    , data = Segment 20 (Zone 5 inf)
                    }
              }
            ]
        }

        |> changed
      )

    ]
