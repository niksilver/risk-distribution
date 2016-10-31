module DerivationScheme exposing
    ( Scheme
    , addSegment
    , addForSegments
    , scheme
    )

import Zone exposing
    ( Zone
    , Change (Subst, Add, NoChange)
    )
import Constraint as Cons exposing (Segment, baseSegment)
import Derivation exposing (Derivation)
import DerivationSet


-- A scheme represents:
-- our segments (our judgements, or claims, over the distribution),
-- the zones, and
-- the derivations


type alias Scheme =
    { segments : List Segment
    , zones : List Zone
    , derivations : List Derivation
    }


-- Given a scheme, add new segments and derive the resulting segments
-- that would go into an updated scheme.

addForSegments: List Segment -> Scheme -> List Segment
addForSegments segs scheme =
    List.append scheme.segments segs


-- Given a scheme, add a new segment, and adjust the zones and derivations
-- accordingly

addSegment : Segment -> Scheme -> Scheme
addSegment seg scheme =
    let
        zones = scheme.zones
        changes = Zone.overlay seg.zone zones
    in
        scheme
            |> addSegmentJustSegment seg
            |> addSegmentJustZones changes
            |> addSegmentJustDerivations changes

addSegmentJustSegment : Segment -> Scheme -> Scheme
addSegmentJustSegment seg scheme =
    { scheme
    | segments = List.concat [scheme.segments, [seg]]
    }

addSegmentJustZones : List Change -> Scheme -> Scheme
addSegmentJustZones changes scheme =
    { scheme
    | zones  = List.foldl Zone.apply scheme.zones changes
    }

addSegmentJustDerivations : List Change -> Scheme -> Scheme
addSegmentJustDerivations changes scheme =
    { scheme
    | derivations = List.map (applyAllToCoeffs changes) scheme.derivations
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

-- Build a scheme from segments.
-- The base segment will be added at the start, before those given.

scheme : List Segment -> Scheme
scheme segments =
    let
        initial = Scheme [] [] []
    in
        List.foldl scheme' initial (baseSegment :: segments)

scheme' : Segment -> Scheme -> Scheme
scheme' segment sch =
    let
        srcId = List.length sch.segments
        scheme2 = addSegment segment sch
        cons = Cons.constraint segment scheme2.zones
        deriv = Derivation cons [srcId]
    in
        { scheme2
        | derivations = DerivationSet.deriveAllWithLists scheme2.derivations deriv
        }
