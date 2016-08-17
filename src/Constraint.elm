module Constraint exposing
    ( Segment, baseSegment
    , Constraint, constraintToString
    , isSubcoeff
    , applyToCoeffs
    , constraint, subtract
    )

import Util
import Zone exposing
    ( Zone
    , inf
    , Change (Subst, Add, NoChange)
    )

import String

{- How to construct and deduce a distribution.

* Start with a single zone of -inf to +inf at 100%.
  From this we get a constraint A = 100

* Introduce a new judgement of an interval being at a certain %age.
  E.g. "It's 40% likely it's <= 0"
  This is a "segment".

* Use this to split existing zones.
  E.g. Zone A from -inf to +inf is split into
  zone B of -inf to 0 and zone C of 0 to +inf.

* Replace previous variables in constraints with the split variables
  where appropriate.
  E.g. constraint A = 100 becomes B + C = 100

* Add a new constraint matching the new judgement.
  E.g. we add constraint B = 40.

* [X] Find where the LHS of this new constraint is a subset or superset
  of any previous constraint.
  E.g. B is a strict subset of B + C.

* For the new subset/superset pair add a new constraint by subtraction.
  E.g. (B + C = 100) - (B = 40) gives us (C = 60)

* For each new constraint repeat from step [X]. Stop only when there are
  no more new constraints.

* Now we have to find the values of the remaining variables which don't
  have constraints of the form X = [some int]

 -}

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

-- Coeffivient A is a subcoefficient of B if they're the same length
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

-- Apply a zone Change to a list of coefficients.

applyToCoeffs : Change -> List Int -> List Int
applyToCoeffs change coeffs =
    case change of
        Add idx _ ->
            Util.insert idx [0] coeffs
        Subst idx new ->
            applySubstToCoeffs idx new coeffs
        NoChange ->
            coeffs

applySubstToCoeffs : Int -> List Zone -> List Int -> List Int
applySubstToCoeffs idx new coeffs =
    let
        numZones = List.length new
    in
        case (Util.nth idx coeffs) of
            Just c ->
                Util.spliceOne idx (List.repeat numZones c) coeffs
            Nothing ->
                coeffs

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
