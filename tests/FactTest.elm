module FactTest exposing (all)

import Fact exposing (..)

import Zone exposing (inf, Zone)
import Segment exposing (Segment)

import ElmTest exposing (..)

all : Test
all =
    suite "FactTest"
    [ initTest
    , resetTest
    , updateTestForProbPerc
    , updateTestForLower
    , updateTestForUpper
    , updateTestForChangeLimit
    , updateTestForConfirmText
    , updateTestForResetText
    , changedTestForLowerBound
    , changedTestForUpperBound
    , changedTestForLowerAndUpperBound
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

resetTest : Test
resetTest =
    suite "resetTest"

    [ test "Resetting unchanged fact should keep it the same" <|
      assertEqual
      { text = { probPerc = "40", limit = Between, lower = "10", upper = "50" }
      , data = Segment 40 (Zone 10 50)
      }
      ( { text = { probPerc = "40", limit = Between, lower = "10", upper = "50" }
        , data = Segment 40 (Zone 10 50)
        }
            |> reset
      )

    , test "Resetting Between fact when all text is changed should reset all fields" <|
      assertEqual
      { text = { probPerc = "50", limit = Between, lower = "0", upper = "2000" }
      , data = Segment 50 (Zone 0 2000)
      }
      ( { text = { probPerc = "", limit = AtLeast, lower = "", upper = "" }
        , data = Segment 50 (Zone 0 2000)
        }
            |> reset
      )

    , test "Resetting AtLeast fact when all text is changed should change all, and upper should be empty" <|
      assertEqual
      { text = { probPerc = "10", limit = AtLeast, lower = "1", upper = "" }
      , data = Segment 10 (Zone 1 inf)
      }
      ( { text = { probPerc = "", limit = Between, lower = "", upper = "10" }
        , data = Segment 10 (Zone 1 inf)
        }
            |> reset
      )

    , test "Resetting AtMost fact when all text is changed should change all, and lower should be empty" <|
      assertEqual
      { text = { probPerc = "15", limit = AtMost, lower = "", upper = "1000" }
      , data = Segment 15 (Zone -inf 1000)
      }
      ( { text = { probPerc = "", limit = Between, lower = "0", upper = "" }
        , data = Segment 15 (Zone -inf 1000)
        }
            |> reset
      )

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

updateTestForResetText : Test
updateTestForResetText =
    suite "updateTestForResetText"

    [ test "Resetting unchanged fact should keep it the same" <|
      assertEqual
      { text = { probPerc = "40", limit = Between, lower = "10", upper = "50" }
      , data = Segment 40 (Zone 10 50)
      }
      ( { text = { probPerc = "40", limit = Between, lower = "10", upper = "50" }
        , data = Segment 40 (Zone 10 50)
        }
            |> update ResetText
      )

    , test "Resetting Between fact when all text is changed should reset all fields" <|
      assertEqual
      { text = { probPerc = "50", limit = Between, lower = "0", upper = "2000" }
      , data = Segment 50 (Zone 0 2000)
      }
      ( { text = { probPerc = "", limit = AtLeast, lower = "", upper = "" }
        , data = Segment 50 (Zone 0 2000)
        }
            |> update ResetText
      )

    , test "Resetting AtLeast fact when all text is changed should change all, and upper should be empty" <|
      assertEqual
      { text = { probPerc = "10", limit = AtLeast, lower = "1", upper = "" }
      , data = Segment 10 (Zone 1 inf)
      }
      ( { text = { probPerc = "", limit = Between, lower = "", upper = "10" }
        , data = Segment 10 (Zone 1 inf)
        }
            |> update ResetText
      )

    , test "Resetting AtMost fact when all text is changed should change all, and lower should be empty" <|
      assertEqual
      { text = { probPerc = "15", limit = AtMost, lower = "", upper = "1000" }
      , data = Segment 15 (Zone -inf 1000)
      }
      ( { text = { probPerc = "", limit = Between, lower = "0", upper = "" }
        , data = Segment 15 (Zone -inf 1000)
        }
            |> update ResetText
      )

    ]

changedTestForLowerBound : Test
changedTestForLowerBound =
    suite "changedTestForLowerBound"

    [ test "A consistent AtLeast fact should not be seen as changed" <|
      assertEqual
      (False)
      ( { text = { probPerc = "50", limit = AtLeast, lower = "0", upper = "" }
        , data = Segment 50 (Zone 0 inf)
        }
            |> changed
      )

    , test "A consistent AtLeast fact with bad upper text should not be seen as changed" <|
      assertEqual
      (False)
      ( { text = { probPerc = "10", limit = AtLeast, lower = "0", upper = "x" }
        , data = Segment 10 (Zone 0 inf)
        }
            |> changed
      )

    , test "A consistent AtLeast fact with some upper value should not be seen as changed" <|
      assertEqual
      (False)
      ( { text = { probPerc = "10", limit = AtLeast, lower = "0", upper = "1" }
        , data = Segment 10 (Zone 0 inf)
        }
            |> changed
      )

    , test "An AtLeast fact with changed prob should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "10", limit = AtLeast, lower = "0", upper = "" }
        , data = Segment 50 (Zone 0 inf)
        }
            |> changed
      )

    , test "An AtLeast fact with changed lower text should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "15", limit = AtLeast, lower = "5", upper = "" }
        , data = Segment 15 (Zone 0 inf)
        }
            |> changed
      )

    , test "An AtLeast fact with bad lower text should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "15", limit = AtLeast, lower = " 5", upper = "" }
        , data = Segment 15 (Zone 5 inf)
        }
            |> changed
      )

    , test "An AtLeast fact with bad prob text should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "_15", limit = AtLeast, lower = "5", upper = "" }
        , data = Segment 15 (Zone 5 inf)
        }
            |> changed
      )

    , test "An AtLeast fact with AtMost limit should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "15", limit = AtMost, lower = "10", upper = "" }
        , data = Segment 15 (Zone 10 inf)
        }
            |> changed
      )

    , test "An AtLeast fact with Between limit should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "15", limit = Between, lower = "10", upper = "" }
        , data = Segment 15 (Zone 10 inf)
        }
            |> changed
      )

    ]

changedTestForUpperBound : Test
changedTestForUpperBound =
    suite "changedTestForUpperBound"

    [ test "A consistent AtMost fact should not be seen as changed" <|
      assertEqual
      (False)
      ( { text = { probPerc = "15", limit = AtMost, lower = "", upper = "20" }
        , data = Segment 15 (Zone -inf 20)
        }
            |> changed
      )

    , test "A consistent AtMost fact with bad lower text should not be seen as changed" <|
      assertEqual
      (False)
      ( { text = { probPerc = "15", limit = AtMost, lower = "x", upper = "20" }
        , data = Segment 15 (Zone -inf 20)
        }
            |> changed
      )

    , test "A consistent AtMost fact with some lower value should not be seen as changed" <|
      assertEqual
      (False)
      ( { text = { probPerc = "15", limit = AtMost, lower = "10", upper = "20" }
        , data = Segment 15 (Zone -inf 20)
        }
            |> changed
      )

    , test "An AtMost fact with different prob value should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "44", limit = AtMost, lower = "", upper = "20" }
        , data = Segment 15 (Zone -inf 20)
        }
            |> changed
      )

    , test "An AtMost fact with bad prob text should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "", limit = AtMost, lower = "", upper = "20" }
        , data = Segment 15 (Zone -inf 20)
        }
            |> changed
      )

    , test "An AtMost fact with different upper value should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "15", limit = AtMost, lower = "", upper = "35" }
        , data = Segment 15 (Zone -inf 20)
        }
            |> changed
      )

    , test "An AtMost fact with bad upper text should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "15", limit = AtMost, lower = "", upper = "x" }
        , data = Segment 15 (Zone -inf 20)
        }
            |> changed
      )

    , test "An AtMost fact with AtLeast limit should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "15", limit = AtLeast, lower = "", upper = "20" }
        , data = Segment 15 (Zone -inf 20)
        }
            |> changed
      )

    , test "An AtMost fact with Between limit should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "15", limit = Between, lower = "", upper = "20" }
        , data = Segment 15 (Zone -inf 20)
        }
            |> changed
      )

    ]

changedTestForLowerAndUpperBound : Test
changedTestForLowerAndUpperBound =
    suite "changedTestForLowerAndUpperBound"

    [ test "A consistent wholly bound fact should not be seen as changed" <|
      assertEqual
      (False)
      ( { text = { probPerc = "50", limit = Between, lower = "10", upper = "20" }
        , data = Segment 50 (Zone 10 20)
        }
            |> changed
      )

    , test "A wholly bound fact with bad prob text should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "", limit = Between, lower = "10", upper = "20" }
        , data = Segment 50 (Zone 10 20)
        }
            |> changed
      )

    , test "A wholly bound fact with different prob value should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "33", limit = Between, lower = "10", upper = "20" }
        , data = Segment 50 (Zone 10 20)
        }
            |> changed
      )

    , test "A wholly bound fact with bad lower text should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "50", limit = Between, lower = "", upper = "20" }
        , data = Segment 50 (Zone 10 20)
        }
            |> changed
      )

    , test "A wholly bound fact with different lower value should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "50", limit = Between, lower = "12", upper = "20" }
        , data = Segment 50 (Zone 10 20)
        }
            |> changed
      )

    , test "A wholly bound fact with bad upper text should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "50", limit = Between, lower = "10", upper = "x" }
        , data = Segment 50 (Zone 10 20)
        }
            |> changed
      )

    , test "A wholly bound fact with different upper value should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "50", limit = Between, lower = "10", upper = "25" }
        , data = Segment 50 (Zone 10 20)
        }
            |> changed
      )

    , test "A wholly bound fact with AtLeast limit should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "50", limit = AtLeast, lower = "10", upper = "20" }
        , data = Segment 50 (Zone 10 20)
        }
            |> changed
      )

    , test "A wholly bound fact with AtMost limit should be seen as changed" <|
      assertEqual
      (True)
      ( { text = { probPerc = "50", limit = AtMost, lower = "10", upper = "20" }
        , data = Segment 50 (Zone 10 20)
        }
            |> changed
      )


    ]
