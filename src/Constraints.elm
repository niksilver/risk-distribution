module Constraints exposing
    ( inf, Zone, baseZone, isSubzone
    , Segment
    , Relation (NoRelation, Before, OnEdgeOf, Inside), relativeTo, isInside
    , Change (Subst, Add, NoChange)
    , splitOne, split
    , overlayOnce, overlay
    , apply
    , Constraint, constraintToString
    , isSubcoeff
    , Model
    , addSegment, applyToCoeffs
    , constraint, addConstraint, subtract
    , deriveOnce
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

-- See if a zone is within another. A zone is a subzone of itself.

isSubzone : Zone -> Zone -> Bool
isSubzone small large =
    large.from <= small.from
    && small.to <= large.to

-- A segemnt of our distribution representing the idea that, say,
-- '40% of the distribution lies between -10 and +10'

type alias Segment =
    { pc : Int
    , zone : Zone
    }

-- How a point might relate to the next zone in a sequence
-- We assume the zones are in order.

type Relation
    = NoRelation
    | Before Zone
    | OnEdgeOf Zone
    | Inside Zone

relativeTo : Float -> List Zone -> Relation
relativeTo x zones =
    case zones of
        [] ->
            NoRelation
        head :: tail ->
            if (x < head.from) then
                Before head
            else if (x == head.from) then
                OnEdgeOf head
            else if (x < head.to) then
                Inside head
            else
                relativeTo x tail

-- See if a value is strictly inside a zone

isInside : Float -> Zone -> Bool
isInside x zone =
    (zone.from < x && x < zone.to)

-- Description of a change to a list of zones:
-- We either want to add a zone or substitute a zone.
-- When we split a list of zones we want to know which one (its index)
-- we substitute with which new zones.
-- When we add a zone to a list of zones we want to know where it goes
-- (its index) and what it is.

type Change
    = Subst Int (List Zone)
    | Add Int Zone
    | NoChange

-- Take a zone and split it if a given value is inside it

splitOne : Float -> Zone -> Maybe (List Zone)
splitOne x zone =
    if (isInside x zone) then
        Just [ Zone zone.from x, Zone x zone.to ]
    else
        Nothing

-- Find a split, if there is one, in a list of zones

split : Float -> List Zone -> Change
split x zones =
    split' 0 x zones

split' : Int -> Float -> List Zone -> Change
split' idx x zones =
    case zones of
        [] ->
            NoChange
        zone :: tail ->
            case (splitOne x zone) of
                Nothing ->
                    split' (idx + 1) x tail
                Just new ->
                    Subst idx new

-- Overlay a new zone onto a series of others (which are assumed to be
-- ordered). We return the next one zone change only,
-- and possibly any zone left over.

overlayOnce : Zone -> List Zone -> (Change, Maybe Zone)
overlayOnce zone zones =
    case (relativeTo zone.from zones) of
        NoRelation ->
            overlayOnceNoRelation zone zones
        Before next ->
            overlayOnceBefore zone next zones
        Inside curr ->
            overlayOnceInside zone curr zones
        OnEdgeOf next ->
            overlayOnceOnEdgeOf zone next zones

overlayOnceNoRelation : Zone -> List Zone -> (Change, Maybe Zone)
overlayOnceNoRelation zone zones =
    let
        idx = List.length zones
    in
        (Add idx zone, Nothing)

overlayOnceBefore : Zone -> Zone -> List Zone -> (Change, Maybe Zone)
overlayOnceBefore zone next zones =
    let
        idx = Util.indexOf next zones |> Maybe.withDefault 0
        to' = min zone.to next.from
        zone' = Zone zone.from to'
        remainder =
            if (to' == zone.to) then
                Nothing
            else
                Just (Zone to' zone.to)
    in
        (Add idx zone', remainder)

overlayOnceInside : Zone -> Zone -> List Zone -> (Change, Maybe Zone)
overlayOnceInside zone curr zones =
    let
        idx = Util.indexOf curr zones |> Maybe.withDefault 0
        zone1 = Zone curr.from zone.from
        zone2 = Zone zone.from curr.to
        remainder =
            if (curr.to >= zone.to) then
                Nothing
            else
                Just (Zone curr.to zone.to)
    in
        (Subst idx [zone1, zone2], remainder)

overlayOnceOnEdgeOf : Zone -> Zone -> List Zone -> (Change, Maybe Zone)
overlayOnceOnEdgeOf zone next zones =
    let
        remainder =
            if (zone.to <= next.to) then
                Nothing
            else
                Just (Zone next.to zone.to)
    in
        (NoChange, remainder)

-- Overlay a zone onto a list of other zones, and return what
-- changes need to be made to the original zones.
-- Assumes the other zones are in order.
-- Note that the returned changes have to be used together,
-- because adding one zone or substituting one zone for two
-- others will changes the indexes required of later ones.

overlay : Zone -> List Zone -> List Change
overlay zone zones =
    let
        (revChanges, zones') = overlay' zone zones []
        change = split zone.to zones'
    in
        change :: revChanges
            |> List.filter ((/=) NoChange)
            |> List.reverse

overlay' : Zone -> List Zone -> List Change -> (List Change, List Zone)
overlay' zone zones changes =
    let
        (change, maybeZone) = overlayOnce zone zones
        zones' = apply change zones
        changes' = change :: changes
    in
        case maybeZone of
            Just zone' ->
                overlay' zone' zones' changes'
            Nothing ->
                (changes', zones')

-- Apply a change to some zones

apply : Change -> List Zone -> List Zone
apply change zones =
    case change of
        Add idx new ->
            Util.insert idx [new] zones
        Subst idx new ->
            Util.spliceOne idx new zones
        NoChange ->
            zones

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
    let
        zones = model.zones
        changes = overlay seg.zone zones
    in
        model
            |> addSegmentJustSegment seg
            |> addSegmentJustZones changes
            |> addSegmentJustConstraints changes

addSegmentJustSegment : Segment -> Model -> Model
addSegmentJustSegment seg model =
    { model
    | segments = List.concat [model.segments, [seg]]
    }

addSegmentJustZones : List Change -> Model -> Model
addSegmentJustZones changes model =
    { model
    | zones  = List.foldl apply model.zones changes
    }

addSegmentJustConstraints : List Change -> Model -> Model
addSegmentJustConstraints changes model =
    { model
    | constraints = List.map (applyAllToCoeffs changes) model.constraints
    }

applyAllToCoeffs : List Change -> Constraint -> Constraint
applyAllToCoeffs changes constraint =
    { constraint
    | coeffs = List.foldl applyToCoeffs constraint.coeffs changes
    }

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

-- Create a Constraint

constraint : Segment -> List Zone -> Constraint
constraint seg zones =
    let
        coeff zone =
            if (isSubzone zone seg.zone) then 1 else 0
    in
        Constraint (List.map coeff zones) seg.pc

-- Add a constraint to a model

addConstraint : Constraint -> Model -> Model
addConstraint constraint model =
    { model
    | constraints = List.append model.constraints [constraint]
    }

-- Take one constraint and subtract another

subtract : Constraint -> Constraint -> Constraint
subtract larger smaller =
    { coeffs = List.map2 (-) larger.coeffs smaller.coeffs
    , pc = larger.pc - smaller.pc
    }

-- Derive more constraints given some existing ones
-- using another to subtract.

deriveOnce : List Constraint -> Constraint -> List Constraint
deriveOnce constraints seed =
    let
        maybeMap c =
            if (isSubcoeff seed.coeffs c.coeffs) then
                Just (subtract c seed)
            else if (isSubcoeff c.coeffs seed.coeffs) then
                Just (subtract seed c)
            else
                Nothing
    in
        List.filterMap maybeMap constraints
