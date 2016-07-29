module Constraints exposing
    ( inf, Zone, baseZone
    , Segment
    , Relation (Outside, Inside, Edge), relativeTo
    , Subst, splitOne, split
    , Constraint, constraintToString
    )

import Util

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

inf : Float
inf = 1/0

-- A Zone is a range along the -inf/+inf line which will have some
-- part of the distribution curve.

type alias Zone = { from : Float, to : Float}

baseZone = Zone -inf inf

-- A segemnt of our distribution representing the idea that, say,
-- '40% of the distribution lies between -10 and +10'

type alias Segment =
    { pc : Int
    , zone : Zone
    }

-- How a value is positioned relative to a zone.

type Relation
    = Inside
    | Outside
    | Edge

-- See where a float is relative to a given zone

relativeTo : Float -> Zone -> Relation
relativeTo x zone =
    if (x < zone.from) then
        Outside
    else if (x > zone.to) then
        Outside
    else if (x == zone.from) then
        Edge
    else if (x == zone.to) then
        Edge
    else
        Inside

-- When we split a list of zones we want to know which one (its index)
-- we substitute with which new zones

type alias Subst =
    { index : Int
    , new : List Zone
    }

-- Take a zone and split it if a given value is inside it (otherwise leave it)

splitOne : Float -> Zone -> List Zone
splitOne x zone =
    case (relativeTo x zone) of
        Inside ->
            [ Zone zone.from x, Zone x zone.to ]
        _ ->
            [ zone ]

-- Find a split, if there is one, in a list of zones

split : Float -> List Zone -> Maybe Subst
split x zones =
    split' 0 x zones

split' : Int -> Float -> List Zone -> Maybe Subst
split' idx x zones =
    case zones of
        [] ->
            Nothing
        zone :: tail ->
            case (splitOne x zone) of
                [ _ ] ->
                    split' (idx + 1) x tail
                new ->
                    Just (Subst idx new)

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

toLetter : Int -> String
toLetter idx =
    let
        az = "abcdefghijklmnopqrstuvwxyz"
    in
        if (0 <= idx && idx <= String.length az) then
            String.slice idx (idx+1) az
        else
            "?"

constraintToStringLHS : List Int -> String
constraintToStringLHS coeffs =
    let
        idxCoeffs = List.indexedMap (,) coeffs

        idxOfNonZeroCoeff (idx, coeff) =
            if (coeff == 0) then Nothing else Just idx

        idxOfFirstNonZeroCoeff =
            Util.find idxOfNonZeroCoeff idxCoeffs

        coeffToLetter (idx, coeff) =
            if (coeff == 0) then " " else toLetter idx

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
