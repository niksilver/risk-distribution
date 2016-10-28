module ConstraintTest exposing (all)

import Constraint exposing (..)
import Zone exposing
    ( Zone, inf
    , Change (Subst, Add, NoChange)
    )

import ElmTest exposing (..)

all : Test
all =
    suite "ConstraintTest"
    [ constraintToStringTest
    , applyToCoeffsTest
    , constraintTest
    , isSubcoeffTest
    , subtractTest
    , isContradictionTest
    ]

constraintToStringTest : Test
constraintToStringTest =
    suite "constraintToStringTest"

    [ test "Should work for 'a = 40' (with just a known)" <|
      assertEqual
      "a = 40"
      (Constraint [1] 40 |> constraintToString)

    , test "Should work for 'a = 40' (with a and b known)" <|
      assertEqual
--     a + b = ..
      "a     = 40"
      (Constraint [1, 0] 40 |> constraintToString)

    , test "Should work for 'a = 40' (with a, b and c known)" <|
      assertEqual
--     a + b + c = ..
      "a         = 40"
      (Constraint [1, 0, 0] 40 |> constraintToString)

    , test "Should work for 'a + b = 60'" <|
      assertEqual
--     a + b + c = ..
      "a + b     = 60"
      (Constraint [1, 1, 0] 60 |> constraintToString)

    , test "Should work for 'a + c = 33'" <|
      assertEqual
--     a + b + c = ..
      "a     + c = 33"
      (Constraint [1, 0, 1] 33 |> constraintToString)

    , test "Should work for 'a + b + c = 65'" <|
      assertEqual
      "a + b + c = 65"
      (Constraint [1, 1, 1] 65 |> constraintToString)

    , test "Should work for 'b = 10'" <|
      assertEqual
--     a + b + c = ..
      "    b     = 10"
      (Constraint [0, 1, 0] 10 |> constraintToString)

    , test "Should work for 'b + c = 15'" <|
      assertEqual
--     a + b + c = ..
      "    b + c = 15"
      (Constraint [0, 1, 1] 15 |> constraintToString)

    , test "Should work for 'c = 4'" <|
      assertEqual
--     a + b + c = ..
      "        c = 4"
      (Constraint [0, 0, 1] 4 |> constraintToString)

    ]

applyToCoeffsTest : Test
applyToCoeffsTest =
    suite "applyToCoeffsTest"

    [ test "Applying Add at the start should work" <|
      assertEqual
      [0, 1, 1]
      (applyToCoeffs (Add 0 (Zone -10 0)) [1, 1])

    , test "Applying Add at the start of an empty list should work" <|
      assertEqual
      [0]
      (applyToCoeffs (Add 0 (Zone -10 0)) [])

    , test "Applying Add in the middle should work" <|
      assertEqual
      [1, 0, 1, 1]
      (applyToCoeffs (Add 1 (Zone 10 15)) [1, 1, 1])

    , test "Applying Add at the end should work" <|
      assertEqual
      [1, 1, 1, 0]
      (applyToCoeffs (Add 3 (Zone 30 40)) [1, 1, 1])

    , test "Applying Add at a negative point should have no effect" <|
      assertEqual
      [1, 0, 1]
      (applyToCoeffs (Add -1 (Zone 30 40)) [1, 0, 1])

    , test "Applying Add beyond the end should have no effect" <|
      assertEqual
      [1, 0, 1]
      (applyToCoeffs (Add 4 (Zone 30 40)) [1, 0, 1])

      , test "Applying Subst two at the start for coeff 0 should work" <|
      assertEqual
      [0, 0, 1]
      (applyToCoeffs (Subst 0 [Zone 0 5, Zone 5 10]) [0, 1])

      , test "Applying Subst two at the start for coeff 1 should work" <|
      assertEqual
      [1, 1, 0]
      (applyToCoeffs (Subst 0 [Zone 0 5, Zone 5 10]) [1, 0])

      , test "Applying Subst three at the start for coeff 1 should work" <|
      assertEqual
      [1, 1, 1, 0]
      (applyToCoeffs (Subst 0 [Zone 0 5, Zone 5 10, Zone 10 15]) [1, 0])

    , test "Applying Subst at index 0 of an empty list should have no effect" <|
      assertEqual
      []
      (applyToCoeffs (Subst 0 [Zone 0 5, Zone 5 10]) [])

    , test "Applying Subst two in the middle for coeff 0 should work" <|
      assertEqual
      [1, 0, 0, 1]
      (applyToCoeffs (Subst 1 [Zone 10 15, Zone 15 20]) [1, 0, 1])

    , test "Applying Subst two in the middle for coeff 1 should work" <|
      assertEqual
      [0, 1, 1, 0]
      (applyToCoeffs (Subst 1 [Zone 10 15, Zone 15 20]) [0, 1, 0])

    , test "Applying Subst three in the middle for coeff 0 should work" <|
      assertEqual
      [1, 0, 0, 0, 1]
      (applyToCoeffs (Subst 1 [Zone 10 15, Zone 15 20, Zone 20 25]) [1, 0, 1])

    , test "Applying Subst three in the middle for coeff 1 should work" <|
      assertEqual
      [0, 1, 1, 1, 0]
      (applyToCoeffs (Subst 1 [Zone 10 15, Zone 15 20, Zone 20 25]) [0, 1, 0])

    , test "Applying Subst at the end should work" <|
      assertEqual
      [1, 0, 1, 1]
      (applyToCoeffs (Subst 2 [Zone 20 25, Zone 25 30]) [1, 0, 1])

    , test "Applying Subst at a negative point should have no effect" <|
      assertEqual
      [1, 1, 0]
      (applyToCoeffs (Subst -1 [Zone -10 -5, Zone -5 0]) [1, 1, 0])

    , test "Applying Subst beyond the end should have no effect" <|
      assertEqual
      [0, 1, 1]
      (applyToCoeffs (Subst 3 [Zone 30 35, Zone 35 40]) [0, 1, 1])

    , test "Applying NoChange should have no effect" <|
      assertEqual
      [0, 1, 1]
      (applyToCoeffs NoChange [0, 1, 1])

    ]

constraintTest : Test
constraintTest =
    suite "constraintTest"

    [ test "A constraint at the start should work" <|
      assertEqual
      (Constraint [1, 1, 0] 45)
      (constraint
        (Segment 45 (Zone -10 10))
        [Zone -10 0, Zone 0 10, Zone 10 20]
      )

    , test "A constraint in the middle should work" <|
      assertEqual
      (Constraint [0, 1, 1, 0] 22)
      (constraint
        (Segment 22 (Zone 0 15))
        [Zone -10 0, Zone 0 10, Zone 10 15, Zone 15 20]
      )

    , test "A constraint at the end should work" <|
      assertEqual
      (Constraint [0, 0, 1, 1] 33)
      (constraint
        (Segment 33 (Zone 10 20))
        [Zone -10 0, Zone 0 10, Zone 10 15, Zone 15 20]
      )

    , test "A constraint covering one zone on the left should work" <|
      assertEqual
      (Constraint [1, 0, 0, 0] 44)
      (constraint
        (Segment 44 (Zone -10 0))
        [Zone -10 0, Zone 0 10, Zone 10 15, Zone 15 20]
      )

    , test "A constraint covering one zone on the right should work" <|
      assertEqual
      (Constraint [0, 0, 0, 1] 55)
      (constraint
        (Segment 55 (Zone 15 20))
        [Zone -10 0, Zone 0 10, Zone 10 15, Zone 15 20]
      )

    , test "A constraint covering all zones should work" <|
      assertEqual
      (Constraint [1, 1, 1, 1] 66)
      (constraint
        (Segment 66 (Zone -10 20))
        [Zone -10 0, Zone 0 10, Zone 10 15, Zone 15 20]
      )

    ]

isSubcoeffTest : Test
isSubcoeffTest =
    suite "isSubcoeffTest"

    [ test "Smaller list is not subcoeff of larger list" <|
      assertEqual
      False
      (isSubcoeff [1, 1] [1, 1, 1])

    , test "Larger list is not subcoeff of smaller list" <|
      assertEqual
      False
      (isSubcoeff [1, 1, 1] [1, 1])

    , test "List is subcoeff of itself" <|
      assertEqual
      True
      (isSubcoeff [1, 1, 0] [1, 1, 0])

    , test "All zeros is subcoeff of anything with non-zeros (and same length)" <|
      assertEqual
      True
      (isSubcoeff [0, 0, 0] [1, 1, 0])

    , test "Something with non-zeros is subcoeff of that plus other non-zeros" <|
      assertEqual
      True
      (isSubcoeff [1, 0, 0, 2, 0] [1, 0, 1, 2, 0])

    ]

subtractTest : Test
subtractTest =
    suite "subtractTest"

    [ test "Basic subtraction should work" <|
      assertEqual
      (Constraint [1, 3, -3] 12)
      (subtract (Constraint [2, 3, 4] 90) (Constraint [1, 0, 7] 78))

    , test "Longer subtract shorter should ignore extra coefficients" <|
      assertEqual
      (Constraint [-2, 2] 4)
      (subtract (Constraint [2, 3, 4, 1] 10) (Constraint [4, 1] 6))

    , test "Shorter subtract longer should ignore extra coefficients" <|
      assertEqual
      (Constraint [2, -2] 3)
      (subtract (Constraint [4, 1] 20) (Constraint [2, 3, 4, 5] 17))

    ]

isContradictionTest : Test
isContradictionTest =
    suite "isContradictionTest"

    [ test "A constraint does not contradict itself" <|
      assertEqual
      False
      (isContradiction
        (Constraint [0, 1, 1, 0] 20)
        (Constraint [0, 1, 1, 0] 20)
      )

    , test "Constraints with different coefficients don't contradict" <|
      assertEqual
      False
      (isContradiction
        (Constraint [0, 1, 1, 0] 40)
        (Constraint [0, 1, 0, 0] 40)
      )

    , test "Constraints with same coefficients but different percentages do contradict" <|
      assertEqual
      True
      (isContradiction
        (Constraint [0, 1, 0, 0] 30)
        (Constraint [0, 1, 0, 0] 35)
      )

    ]
