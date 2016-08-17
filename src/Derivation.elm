module Derivation exposing
    ( Derivation, derivationToString
    , subtract
    , deduceOnce, deduceAll
    , Model --, model
    , addSegment
    )

import Zone exposing
    ( Zone
    , Change (Subst, Add, NoChange)
    )
import Constraint as Cons exposing (Segment, baseSegment, Constraint)
import Util

import String


-- A constraint which also holds the source(s) from which it was derived.

type alias Derivation =
    { cons : Constraint
    , src : List Int
    }

-- The string version of this is just the Constraint plus
-- the source in parentheses

derivationToString : Derivation -> String
derivationToString deriv =
    Cons.constraintToString deriv.cons
    ++ " ("
    ++ (deriv.src |> List.map toString |> String.join ", ")
    ++ ")"

-- Take one derivation and subtract another.
-- The result should include the combined sources

subtract : Derivation -> Derivation -> Derivation
subtract larger smaller =
    Derivation (Cons.subtract larger.cons smaller.cons) (larger.src ++ smaller.src)

-- Deduce more derivations given some existing ones
-- using another to subtract.

deduceOnce : List Derivation -> Derivation -> List Derivation
deduceOnce derivations seed =
    let
        maybeMap d =
            if (seed.cons.coeffs == d.cons.coeffs) then
                Nothing
            else if (Cons.isSubcoeff seed.cons.coeffs d.cons.coeffs) then
                Just (subtract d seed)
            else if (Cons.isSubcoeff d.cons.coeffs seed.cons.coeffs) then
                Just (subtract seed d)
            else
                Nothing
    in
        List.filterMap maybeMap derivations

-- Derive all the derivations we can from some existing ones by
-- adding a new one... including the original ones.

deduceAll : List Derivation -> Derivation -> List Derivation
deduceAll derivations seed =
    let
        equiv der1 der2 =
            der1.cons == der2.cons
    in
        Util.filteredExpand deduceOnce equiv derivations seed

-- A model represents:
-- our segments (our judgements, or claims, over the distribution),
-- the zones, and
-- the derivations

type alias Model =
    { segments : List Segment
    , zones : List Zone
    , derivations : List Derivation
    }

-- Given a model, add a new segment, and adjust the zones and derivations
-- accordingly

addSegment : Segment -> Model -> Model
addSegment seg model =
    let
        zones = model.zones
        changes = Zone.overlay seg.zone zones
    in
        model
            |> addSegmentJustSegment seg
            |> addSegmentJustZones changes
            |> addSegmentJustDerivations changes

addSegmentJustSegment : Segment -> Model -> Model
addSegmentJustSegment seg model =
    { model
    | segments = List.concat [model.segments, [seg]]
    }

addSegmentJustZones : List Change -> Model -> Model
addSegmentJustZones changes model =
    { model
    | zones  = List.foldl Zone.apply model.zones changes
    }

addSegmentJustDerivations : List Change -> Model -> Model
addSegmentJustDerivations changes model =
    { model
    | derivations = List.map (applyAllToCoeffs changes) model.derivations
    }

applyAllToCoeffs : List Change -> Derivation -> Derivation
applyAllToCoeffs changes derivation =
    let
        constraint = derivation.cons
    in
        { derivation
        | cons =
            { constraint
            | coeffs = List.foldl Cons.applyToCoeffs constraint.coeffs changes
            }
        }

-- Build a model from segments.
-- The base segment will be added at the start, before those given.

-- model : List Segment -> Model
-- model segments =
--     let
--         initial = Model [] [] []
--     in
--         List.foldl model' initial (baseSegment :: segments)
--
-- model' : Segment -> Model -> Model
-- model' segment mod =
--     let
--         srcId = List.length mod.segments
--         mod' = Cons.addSegment segment mod
--         cons = Cons.constraint segment mod'.zones
--         deriv = Derivation cons srcId
--     in
--         { mod'
--         | derivations = deduceAll mod'.derivations deriv
--         }
