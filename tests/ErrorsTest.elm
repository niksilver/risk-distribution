module ErrorsTest exposing (all)

import Errors exposing (..)
import Distribution exposing (Layer, Limit(AtMost, AtLeast))

import ElmTest exposing (..)

all : Test
all =
    suite "ErrorsTest"
    [ errorsTestForOkay
    , errorsTestForMoreThan100Percent
    , errorsTestForNoUpperLimit
    , errorsTestForNoLowerLimit
    , errorsTestForContradiction
    , errorsTestForCantMake100Percent
    , indexTest
    ]

errorsTestForOkay : Test
errorsTestForOkay =
    suite "errorsTestForOkay"

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

    ]

errorsTestForMoreThan100Percent : Test
errorsTestForMoreThan100Percent =
    suite "errorsTestForMoreThan100Percent"

    [ test "Layers facing away from each other with > 100% should report error" <|
      let
          y1 = { prob = 0.51, limit = AtMost, value = 20.0 }
          y2 = { prob = 0.60, limit = AtLeast, value = 50.0 }
      in
          assertEqual
          [ MoreThan100Percent 0 1 ]
          (errors [y1, y2])

    , test "Layers facing away from each other reversed with > 100% should report error" <|
      let
          y1 = { prob = 0.60, limit = AtLeast, value = 50.0 }
          y2 = { prob = 0.51, limit = AtMost, value = 20.0 }
      in
          assertEqual
          [ MoreThan100Percent 1 0 ]
          (errors [y1, y2])

    , test "Layers facing away from each other with > 100% and hidden among others should report error" <|
      let
          y1 = { prob = 0.51, limit = AtMost, value = 20.0 }
          y2 = { prob = 0.60, limit = AtLeast, value = 10.0 }
          y3 = { prob = 0.55, limit = AtLeast, value = 50.0 }
      in
          assertEqual
          [ MoreThan100Percent 0 2 ]
          (errors [y1, y2, y3])

    , test "Layers of low value facing away from each other with > 100% should report error" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y2 = { prob = 1.00, limit = AtMost, value = -10.0 }
      in
          assertEqual
          [ MoreThan100Percent 1 0 ]
          (errors [y1, y2])

    , test "Layers at same point facing away from each other with > 100% should report error" <|
      let
          y1 = { prob = 0.20, limit = AtMost, value = 10.0 }
          y2 = { prob = 0.90, limit = AtLeast, value = 10.0 }
      in
          assertEqual
          [ MoreThan100Percent 0 1 ]
          (errors [y1, y2])

    ]

errorsTestForNoUpperLimit : Test
errorsTestForNoUpperLimit =
    suite "errorsTestForNoUpperLimit"

    [ test "Just one AtLeast layer should yield error" <|
      let
          y1 = { prob = 0.60, limit = AtLeast, value = 50.0 }
      in
          assertEqual
          [ NoUpperLimit ]
          (errors [ y1 ])

    , test "Just three AtLeast layers should yield error" <|
      let
          y1 = { prob = 0.60, limit = AtLeast, value = 50.0 }
          y2 = { prob = 0.10, limit = AtLeast, value = 60.0 }
          y3 = { prob = 0.05, limit = AtLeast, value = 70.0 }
      in
          assertEqual
          [ NoUpperLimit ]
          (errors [ y1, y2, y3 ])

    ]

errorsTestForNoLowerLimit : Test
errorsTestForNoLowerLimit =
    suite "errorsTestForNoLowerLimit"

    [ test "Just one AtMost layer should yield error" <|
      let
          y1 = { prob = 0.60, limit = AtMost, value = 50.0 }
      in
          assertEqual
          [ NoLowerLimit ]
          (errors [ y1 ])

    , test "Just three AtMost layers should yield error" <|
      let
          y1 = { prob = 0.05, limit = AtMost, value = 50.0 }
          y2 = { prob = 0.10, limit = AtMost, value = 60.0 }
          y3 = { prob = 0.60, limit = AtMost, value = 70.0 }
      in
          assertEqual
          [ NoLowerLimit ]
          (errors [ y1, y2, y3 ])

    ]


errorsTestForContradiction : Test
errorsTestForContradiction =
    suite "errorsTestForContradiction"

    [ test "Two embedded contradicting 'AtLeast' layers should be exposed" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y2 = { prob = 0.25, limit = AtLeast, value = 60.0 }
          y3 = { prob = 0.50, limit = AtMost, value = 30.0 }
          y4 = { prob = 0.40, limit = AtLeast, value = 65.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ Contradiction 1 3 ]
          (errors [ y1, y2, y3, y4, y5 ])

    , test "Two embedded contradicting 'AtLeast' layers reversed should be exposed" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y2 = { prob = 0.40, limit = AtLeast, value = 65.0 }
          y3 = { prob = 0.50, limit = AtMost, value = 30.0 }
          y4 = { prob = 0.25, limit = AtLeast, value = 60.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ Contradiction 3 1 ]
          (errors [ y1, y2, y3, y4, y5 ])

    , test "Two 'AtLeast' layers of same value and different probs should be a contradition" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y2 = { prob = 0.40, limit = AtLeast, value = 65.0 }
          y3 = { prob = 0.50, limit = AtMost, value = 30.0 }
          y4 = { prob = 0.25, limit = AtLeast, value = 65.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ Contradiction 3 1 ]
          (errors [ y1, y2, y3, y4, y5 ])

    , test "Two 'AtLeast' layers of same value and different probs (reversed) should be a contradition" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y2 = { prob = 0.25, limit = AtLeast, value = 65.0 }
          y3 = { prob = 0.50, limit = AtMost, value = 30.0 }
          y4 = { prob = 0.40, limit = AtLeast, value = 65.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ Contradiction 1 3 ]
          (errors [ y1, y2, y3, y4, y5 ])

    , test "Two embedded contradicting 'AtMost' layers should be exposed" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y2 = { prob = 0.40, limit = AtMost, value = 30.0 }
          y3 = { prob = 0.50, limit = AtLeast, value = 70.0 }
          y4 = { prob = 0.25, limit = AtMost, value = 35.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ Contradiction 1 3 ]
          (errors [ y1, y2, y3, y4, y5 ])

    , test "Two embedded contradicting 'AtMost' layers reversed should be exposed" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y2 = { prob = 0.25, limit = AtMost, value = 35.0 }
          y3 = { prob = 0.50, limit = AtLeast, value = 70.0 }
          y4 = { prob = 0.40, limit = AtMost, value = 30.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ Contradiction 3 1 ]
          (errors [ y1, y2, y3, y4, y5 ])

    , test "Two 'AtMost' layers of same value but different probs should be a contradiction" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y2 = { prob = 0.40, limit = AtMost, value = 35.0 }
          y3 = { prob = 0.50, limit = AtLeast, value = 70.0 }
          y4 = { prob = 0.25, limit = AtMost, value = 35.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ Contradiction 1 3 ]
          (errors [ y1, y2, y3, y4, y5 ])

    , test "Two 'AtMost' layers of same value but different probs (reversed) should be a contradiction" <|
      let
          y1 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y2 = { prob = 0.25, limit = AtMost, value = 35.0 }
          y3 = { prob = 0.50, limit = AtLeast, value = 70.0 }
          y4 = { prob = 0.40, limit = AtMost, value = 35.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ Contradiction 3 1 ]
          (errors [ y1, y2, y3, y4, y5 ])

    ]

errorsTestForCantMake100Percent : Test
errorsTestForCantMake100Percent =
    suite "errorsTestForCantMake100Percent"

    [ test "Two touching layers of less than 100% should be exposed" <|
      let
          y1 = { prob = 0.25, limit = AtLeast, value = 60.0 }
          y2 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y3 = { prob = 0.65, limit = AtMost, value = 60.0 }
          y4 = { prob = 0.20, limit = AtLeast, value = 80.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ CantMake100Percent 0 2 ]
          (errors [ y1, y2, y3, y4, y5 ])

    , test "Two touching layers reversed of less than 100% should be exposed" <|
      let
          y1 = { prob = 0.65, limit = AtMost, value = 60.0 }
          y2 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y3 = { prob = 0.25, limit = AtLeast, value = 60.0 }
          y4 = { prob = 0.20, limit = AtLeast, value = 80.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ CantMake100Percent 2 0 ]
          (errors [ y1, y2, y3, y4, y5 ])

    , test "Two overlapping layers of less than 100% should be exposed" <|
      let
          y1 = { prob = 0.25, limit = AtLeast, value = 60.0 }
          y2 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y3 = { prob = 0.65, limit = AtMost, value = 65.0 }
          y4 = { prob = 0.20, limit = AtLeast, value = 80.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ CantMake100Percent 0 2 ]
          (errors [ y1, y2, y3, y4, y5 ])

    , test "Two overlapping reversed layers of less than 100% should be exposed" <|
      let
          y1 = { prob = 0.65, limit = AtMost, value = 65.0 }
          y2 = { prob = 1.00, limit = AtLeast, value = 0.0 }
          y3 = { prob = 0.25, limit = AtLeast, value = 60.0 }
          y4 = { prob = 0.20, limit = AtLeast, value = 80.0 }
          y5 = { prob = 1.00, limit = AtMost, value = 100.0 }
      in
          assertEqual
          [ CantMake100Percent 2 0 ]
          (errors [ y1, y2, y3, y4, y5 ])

    ]

indexTest : Test
indexTest =
    suite "indexTest"

    [ test "No layers should yield no indexed layers" <|
      assertEqual
      []
      (index [])

    , test "Three layers should yield correct indexing" <|
      let
          y1 = { prob = 0.51, limit = AtMost, value = 20.0 }
          y2 = { prob = 0.60, limit = AtLeast, value = 10.0 }
          y3 = { prob = 0.55, limit = AtLeast, value = 50.0 }
      in
          assertEqual
          [ { layer = y1, index = 0 }
          , { layer = y2, index = 1 }
          , { layer = y3, index = 2 }
          ]
          (index [ y1, y2, y3 ])
    ]
