module FactTest exposing (all)

import Fact exposing (..)

import Zone exposing (inf, Zone)
import Segment exposing (Segment)

import ElmTest exposing (..)

all : Test
all =
    suite "FactTest"
    [ initTest
    , updateTestForProbPerc
    , updateTestForLower
    , updateTestForUpper
    , updateTestForChangeLimit
    , updateTestForConfirmText
    ]

initTest : Test
initTest =
    suite "initTest"

    [ test "init with all zeros and lower bound should be okay" <|
      assertEqual
      { text = { probPerc = "0", limit = AtLeast, lower = "0", upper = "" }
      , data = Segment 0 (Zone 0 inf)
      }
      (init (Segment 0 (Zone 0 inf)))

    , test "init with all zeros and upper bound should be okay" <|
      assertEqual
      { text = { probPerc = "0", limit = AtMost, lower = "", upper = "0" }
      , data = Segment 0 (Zone -inf 0)
      }
      (init (Segment 0 (Zone -inf 0)))

    , test "init with all zeros and range should be okay" <|
      assertEqual
      { text = { probPerc = "0", limit = Between, lower = "0", upper = "0" }
      , data = Segment 0 (Zone 0 0)
      }
      (init (Segment 0 (Zone 0 0)))

    , test "init with other values should be okay" <|
      assertEqual
      { text = { probPerc = "15", limit = Between, lower = "20", upper = "40" }
      , data = Segment 15 (Zone 20 40)
      }
      (init (Segment 15 (Zone 20 40)))

    ]

updateTestForProbPerc : Test
updateTestForProbPerc =
    suite "updateTestForProbPerc"

    [ test "Updating prob perc with non-number should register in text only" <|
      assertEqual
      { text = { probPerc = "hello", limit = Between, lower = "20", upper = "40" }
      , data = Segment 15 (Zone 20 40)
      }
      ( { text = { probPerc = "15", limit = Between, lower = "20", upper = "40" }
        , data = Segment 15 (Zone 20 40)
        }
            |> update (ProbPerc "hello")
      )

    , test "Updating prob perc with number should register in text and data" <|
      assertEqual
      { text = { probPerc = "33", limit = Between, lower = "20", upper = "40" }
      , data = Segment 15 (Zone 20 40)
      }
      ( { text = { probPerc = "15", limit = Between, lower = "20", upper = "40" }
        , data = Segment 15 (Zone 20 40)
        }
            |> update (ProbPerc "33")
      )

    ]

updateTestForLower : Test
updateTestForLower =
    suite "updateTestForLower"

    [ test "Updating lower with non-number should register in text only" <|
      assertEqual
      { text = { probPerc = "20", limit = Between, lower = "hello", upper = "35" }
      , data = Segment 20 (Zone 25 35)
      }
      ( { text = { probPerc = "20", limit = Between, lower = "25", upper = "35" }
        , data = Segment 20 (Zone 25 35)
        }
            |> update (Lower "hello")
      )

    , test "Updating lower with too-high number should register in text only" <|
      assertEqual
      { text = { probPerc = "20", limit = Between, lower = "99", upper = "35" }
      , data = Segment 20 (Zone 25 35)
      }
      ( { text = { probPerc = "20", limit = Between, lower = "25", upper = "35" }
        , data = Segment 20 (Zone 25 35)
        }
            |> update (Lower "99")
      )

    , test "Updating lower with okay number should register in text only" <|
      assertEqual
      { text = { probPerc = "20", limit = Between, lower = "30", upper = "35" }
      , data = Segment 20 (Zone 25 35)
      }
      ( { text = { probPerc = "20", limit = Between, lower = "25", upper = "35" }
        , data = Segment 20 (Zone 25 35)
        }
            |> update (Lower "30")
      )

    ]

updateTestForUpper : Test
updateTestForUpper =
    suite "updateTestForUpper"

    [ test "Updating upper with non-number should register in text only" <|
      assertEqual
      { text = { probPerc = "20", limit = Between, lower = "25", upper = "hello" }
      , data = Segment 20 (Zone 25 35)
      }
      ( { text = { probPerc = "20", limit = Between, lower = "25", upper = "35" }
        , data = Segment 20 (Zone 25 35)
        }
            |> update (Upper "hello")
      )

    , test "Updating upper with too-low number should register in text only" <|
      assertEqual
      { text = { probPerc = "20", limit = Between, lower = "25", upper = "1" }
      , data = Segment 20 (Zone 25 35)
      }
      ( { text = { probPerc = "20", limit = Between, lower = "25", upper = "35" }
        , data = Segment 20 (Zone 25 35)
        }
            |> update (Upper "1")
      )

    , test "Updating upper with okay number should register in text only" <|
      assertEqual
      { text = { probPerc = "20", limit = Between, lower = "25", upper = "66" }
      , data = Segment 20 (Zone 25 35)
      }
      ( { text = { probPerc = "20", limit = Between, lower = "25", upper = "35" }
        , data = Segment 20 (Zone 25 35)
        }
            |> update (Upper "66")
      )

    ]

updateTestForChangeLimit : Test
updateTestForChangeLimit =
    suite "updateTestForChangeLimit"

    [ test "Updating limit AtLeast to AtMost should register in text only and change upper/lower" <|
      assertEqual
      { text = { probPerc = "50", limit = AtMost, lower = "", upper = "10" }
      , data = Segment 50 (Zone 10 inf)
      }
      ( { text = { probPerc = "50", limit = AtLeast, lower = "10", upper = "" }
        , data = Segment 50 (Zone 10 inf)
        }
            |> update (ChangeLimit AtMost)
      )

    , test "Updating limit AtMost to AtLeast should register in text only and change upper/lower" <|
      assertEqual
      { text = { probPerc = "50", limit = AtLeast, lower = "10", upper = "" }
      , data = Segment 50 (Zone -inf 10)
      }
      ( { text = { probPerc = "50", limit = AtMost, lower = "", upper = "10" }
        , data = Segment 50 (Zone -inf 10)
        }
            |> update (ChangeLimit AtLeast)
      )

    , test "Updating limit AtLeast to Between should register in text only" <|
      assertEqual
      { text = { probPerc = "50", limit = Between, lower = "10", upper = "" }
      , data = Segment 50 (Zone 10 inf)
      }
      ( { text = { probPerc = "50", limit = AtLeast, lower = "10", upper = "" }
        , data = Segment 50 (Zone 10 inf)
        }
            |> update (ChangeLimit Between)
      )

    , test "Updating limit Between to AtLeast should register in text only" <|
      assertEqual
      { text = { probPerc = "50", limit = AtLeast, lower = "10", upper = "" }
      , data = Segment 50 (Zone 10 90)
      }
      ( { text = { probPerc = "50", limit = Between, lower = "10", upper = "90" }
        , data = Segment 50 (Zone 10 90)
        }
            |> update (ChangeLimit AtLeast)
      )

    , test "Updating limit AtMost to Between should register in text only" <|
      assertEqual
      { text = { probPerc = "50", limit = Between, lower = "", upper = "10" }
      , data = Segment 50 (Zone -inf 10)
      }
      ( { text = { probPerc = "50", limit = AtMost, lower = "", upper = "10" }
        , data = Segment 50 (Zone -inf 10)
        }
            |> update (ChangeLimit Between)
      )

    , test "Updating limit Between to AtMost should register in text only" <|
      assertEqual
      { text = { probPerc = "50", limit = AtMost, lower = "", upper = "90" }
      , data = Segment 50 (Zone 10 90)
      }
      ( { text = { probPerc = "50", limit = Between, lower = "10", upper = "90" }
        , data = Segment 50 (Zone 10 90)
        }
            |> update (ChangeLimit AtMost)
      )

    ]

updateTestForConfirmText : Test
updateTestForConfirmText =
    suite "updateTestForConfirmText"

    [ test "Updating a Fact with okay prob text should register" <|
      assertEqual
      { text = { probPerc = "50", limit = AtLeast, lower = "0", upper = "" }
      , data = Segment 50 (Zone 0 inf)
      }
      ( { text = { probPerc = "50", limit = AtLeast, lower = "0", upper = "" }
        , data = Segment 0 (Zone 0 inf)
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with okay value text should register" <|
      assertEqual
      { text = { probPerc = "0", limit = AtLeast, lower = "99", upper = "" }
      , data = Segment 0 (Zone 99 inf)
      }
      ( { text = { probPerc = "0", limit = AtLeast, lower = "99", upper = "" }
        , data = Segment 0 (Zone 0 inf)
        }
            |> update ConfirmText
      )

    , test "Updating a Fact from AtLeast to AtMost should register" <|
      assertEqual
      { text = { probPerc = "0", limit = AtMost, lower = "", upper = "0" }
      , data = Segment 0 (Zone -inf 0)
      }
      ( { text = { probPerc = "0", limit = AtMost, lower = "", upper = "0" }
        , data = Segment 0 (Zone 0 inf)
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with several okay changes should register all" <|
      assertEqual
      { text = { probPerc = "40", limit = AtMost, lower = "", upper = "77" }
      , data = Segment 40 (Zone -inf 77)
      }
      ( { text = { probPerc = "40", limit = AtMost, lower = "", upper = "77" }
        , data = Segment 0 (Zone 0 inf)
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with bad prob text and rest good should register none" <|
      assertEqual
      { text = { probPerc = "4x0", limit = AtMost, lower = "77", upper = "" }
      , data = Segment 0 (Zone 0 inf)
      }
      ( { text = { probPerc = "4x0", limit = AtMost, lower = "77", upper = "" }
        , data = Segment 0 (Zone 0 inf)
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with bad lower text and rest good should register none" <|
      assertEqual
      { text = { probPerc = "40", limit = AtMost, lower = "77x", upper = "" }
      , data = Segment 0 (Zone 0 inf)
      }
      ( { text = { probPerc = "40", limit = AtMost, lower = "77x", upper = "" }
        , data = Segment 0 (Zone 0 inf)
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with lower and upper text wrong way round should register none" <|
      assertEqual
      { text = { probPerc = "40", limit = Between, lower = "60", upper = "50" }
      , data = Segment 40 (Zone 10 50)
      }
      ( { text = { probPerc = "40", limit = Between, lower = "60", upper = "50" }
        , data = Segment 40 (Zone 10 50)
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with equal lower and upper text should register none" <|
      assertEqual
      { text = { probPerc = "40", limit = Between, lower = "10", upper = "10" }
      , data = Segment 40 (Zone 10 50)
      }
      ( { text = { probPerc = "40", limit = Between, lower = "10", upper = "10" }
        , data = Segment 40 (Zone 10 50)
        }
            |> update ConfirmText
      )

    ]
