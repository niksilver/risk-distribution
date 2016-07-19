module FactTest exposing (all)

import Fact exposing (..)
import Distribution exposing (Limit(AtMost, AtLeast))

import ElmTest exposing (..)

all : Test
all =
    suite "FactTest"
    [ initTest
    , updateTestForProbPerc
    , updateTestForValue
    , updateTestForChangeLimit
    , updateTestForConfirmText
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

updateTestForProbPerc : Test
updateTestForProbPerc =
    suite "updateTestForProbPerc"

    [ test "Updating prob perc with non-number should register in text only" <|
      assertEqual
      { text = { probPerc = "hello", limit = AtLeast, value = "0" }
      , data = { prob = 0, limit = AtLeast, value = 0 }
      }
      ( { text = { probPerc = "50", limit = AtLeast, value = "0" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update (ProbPerc "hello")
      )

    , test "Updating prob perc with number should register in text only" <|
      assertEqual
      { text = { probPerc = "33", limit = AtLeast, value = "0" }
      , data = { prob = 0, limit = AtLeast, value = 0 }
      }
      ( { text = { probPerc = "50", limit = AtLeast, value = "0" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update (ProbPerc "33")
      )

    ]

updateTestForValue : Test
updateTestForValue =
    suite "updateTestForValue"

    [ test "Updating value with non-number should register in text only" <|
      assertEqual
      { text = { probPerc = "50", limit = AtLeast, value = "hello" }
      , data = { prob = 0, limit = AtLeast, value = 0 }
      }
      ( { text = { probPerc = "50", limit = AtLeast, value = "0" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update (Value "hello")
      )

    , test "Updating value with number should register in text only" <|
      assertEqual
      { text = { probPerc = "50", limit = AtLeast, value = "99" }
      , data = { prob = 0, limit = AtLeast, value = 0 }
      }
      ( { text = { probPerc = "50", limit = AtLeast, value = "0" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update (Value "99")
      )

    ]

updateTestForChangeLimit : Test
updateTestForChangeLimit =
    suite "updateTestForChangeLimit"

    [ test "Updating limit should register in text only" <|
      assertEqual
      { text = { probPerc = "50", limit = AtMost, value = "0" }
      , data = { prob = 0, limit = AtLeast, value = 0 }
      }
      ( { text = { probPerc = "50", limit = AtLeast, value = "0" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update (ChangeLimit AtMost)
      )
    ]

updateTestForConfirmText : Test
updateTestForConfirmText =
    suite "updateTestForConfirmText"

    [ test "Updating a Fact with okay prob text should register" <|
      assertEqual
      { text = { probPerc = "50", limit = AtLeast, value = "0" }
      , data = { prob = 0.50, limit = AtLeast, value = 0 }
      }
      ( { text = { probPerc = "50", limit = AtLeast, value = "0" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with okay value text should register" <|
      assertEqual
      { text = { probPerc = "0", limit = AtLeast, value = "99" }
      , data = { prob = 0, limit = AtLeast, value = 99 }
      }
      ( { text = { probPerc = "0", limit = AtLeast, value = "99" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with new limit should register" <|
      assertEqual
      { text = { probPerc = "0", limit = AtMost, value = "0" }
      , data = { prob = 0, limit = AtMost, value = 0 }
      }
      ( { text = { probPerc = "0", limit = AtMost, value = "0" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with several changes should register all" <|
      assertEqual
      { text = { probPerc = "40", limit = AtMost, value = "77" }
      , data = { prob = 0.40, limit = AtMost, value = 77 }
      }
      ( { text = { probPerc = "40", limit = AtMost, value = "77" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with bad prob text and rest good should register none" <|
      assertEqual
      { text = { probPerc = "4x0", limit = AtMost, value = "77" }
      , data = { prob = 0, limit = AtLeast, value = 0 }
      }
      ( { text = { probPerc = "4x0", limit = AtMost, value = "77" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update ConfirmText
      )

    , test "Updating a Fact with bad value text and rest good should register none" <|
      assertEqual
      { text = { probPerc = "40", limit = AtMost, value = "77x" }
      , data = { prob = 0, limit = AtLeast, value = 0 }
      }
      ( { text = { probPerc = "40", limit = AtMost, value = "77x" }
        , data = { prob = 0, limit = AtLeast, value = 0 }
        }
            |> update ConfirmText
      )

    ]
