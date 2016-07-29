module Constraints exposing
    ( inf, Zone, baseZone
    , Segment
    , isInside
    , Subst, splitOne, split
    , Constraint, constraintToString
    , Model
    , addSegment
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

-- See if a value is strictly inside a zone

isInside : Float -> Zone -> Bool
isInside x zone =
    (zone.from < x && x < zone.to)

-- When we split a list of zones we want to know which one (its index)
-- we substitute with which new zones

type alias Subst =
    { index : Int
    , new : List Zone
    }

-- Take a zone and split it if a given value is inside it

splitOne : Float -> Zone -> Maybe (List Zone)
splitOne x zone =
    if (isInside x zone) then
        Just [ Zone zone.from x, Zone x zone.to ]
    else
        Nothing

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
                Nothing ->
                    split' (idx + 1) x tail
                Just new ->
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

-- A model represents:
-- our segments (our judgements, or claims, over the distribution),
-- the zones, and
-- the constraints

type alias Model =
    { segments : List Segment
    , zones : List Zone
    , constraints : List Constraint
    }

-- Given a model, add a new segment, and adjust the zones and constraints
-- accordingly

addSegment : Segment -> Model -> Model
addSegment seg model =
    model
        |> addSegmentJustSegment seg
        |> addSegmentJustZoneEdge seg.zone.from
        |> addSegmentJustZoneEdge seg.zone.to

addSegmentJustSegment : Segment -> Model -> Model
addSegmentJustSegment seg model =
    { model
    | segments = seg :: model.segments
    }

addSegmentJustZoneEdge : Float -> Model -> Model
addSegmentJustZoneEdge x model =
    case (split x model.zones) of
        Nothing ->
            model
        Just subst ->
            { model
            | zones =
                Util.spliceOne subst.index subst.new model.zones
            , constraints =
                addSegmentJustMapConstraints subst model.constraints
            }

addSegmentJustMapConstraints : Subst -> List Constraint -> List Constraint
addSegmentJustMapConstraints subst cons =
    let
        idx = subst.index
        subsOneCoeffs coeffs =
            case (Util.nth idx coeffs) of
                Just coeff ->
                    Util.spliceOne idx [coeff, coeff] coeffs
                Nothing ->
                    coeffs
        subsOne constr =
            { constr | coeffs = subsOneCoeffs constr.coeffs }
    in
        List.map subsOne cons
        
