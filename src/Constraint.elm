module Constraint exposing
    ( Segment, baseSegment
    , Constraint, constraintToString
    , isSubcoeff
    , constraint, subtract
    , isContradiction
    )

import Util
import Zone exposing
    ( Zone
    , inf
    , Change (Subst, Add, NoChange)
    )

import String


-- A segemnt of our distribution representing the idea that, say,
-- '40% of the distribution lies between -10 and +10'

type alias Segment =
    { pc : Int
    , zone : Zone
    }


-- A segment saying everything must add up to 100%

baseSegment : Segment
baseSegment =
    Segment 100 (Zone -inf inf)

-- A constraint is something of the form
-- a + c + d = 40
-- which is really
-- (1 * x0) + (0 * x1) + (1 * x2) + (1 * x3) = 40
-- which would be represented as the co-efficients and the percent:
-- [ 1, 0, 1, 1 ] 40

type alias Constraint =
    { coeffs : List Int
    , pc : Int
    }

constraintToString : Constraint -> String
constraintToString cons =
    constraintToStringLHS cons.coeffs
    ++ " = "
    ++ (toString cons.pc)

constraintToStringLHS : List Int -> String
constraintToStringLHS coeffs =
    let
        idxCoeffs = List.indexedMap (,) coeffs

        idxOfNonZeroCoeff (idx, coeff) =
            if (coeff == 0) then Nothing else Just idx

        idxOfFirstNonZeroCoeff =
            Util.find idxOfNonZeroCoeff idxCoeffs

        coeffToLetter (idx, coeff) =
            if (coeff == 0) then " " else Util.toLetter idx

        outputIdxCoeff (idx, coeff) =
            if (idx == 0) then
                coeffToLetter (0, coeff)
            else if (coeff == 0) then
                "    "
            else if (Just idx == idxOfFirstNonZeroCoeff) then
                "   " ++ coeffToLetter (idx, coeff)
            else
                " + " ++ coeffToLetter (idx, coeff)
    in
        idxCoeffs
            |> List.map outputIdxCoeff
            |> String.concat

-- Coefficient A is a subcoefficient of B if they're the same length
-- and every non-zero in A has the same non-zero in B.

isSubcoeff : List Int -> List Int -> Bool
isSubcoeff sub super =
    let
        isSub' (c1, c2) =
            c1 == 0 || c1 == c2
        combined =
            List.map2 (,) sub super
    in
        (List.length sub == List.length super)
        && (List.all isSub' combined)

-- Create a Constraint describing a segment, and given our total list
-- of zones

constraint : Segment -> List Zone -> Constraint
constraint seg zones =
    let
        coeff zone =
            if (Zone.isSubzone zone seg.zone) then 1 else 0
    in
        Constraint (List.map coeff zones) seg.pc

-- Take one constraint and subtract another

subtract : Constraint -> Constraint -> Constraint
subtract larger smaller =
    { coeffs = List.map2 (-) larger.coeffs smaller.coeffs
    , pc = larger.pc - smaller.pc
    }

-- Do two constraints contradict?
-- I.e. same coefficients but different percentage

isContradiction : Constraint -> Constraint -> Bool
isContradiction cons1 cons2 =
    cons1.coeffs == cons2.coeffs
        && cons1.pc /= cons2.pc
