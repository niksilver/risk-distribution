module DerivationScheme exposing
    ( Scheme
    , addForSegments, addForZones
    , derivations
    , scheme
    )

import Zone exposing (Zone)
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

-- Given a scheme, add some new segment and derive the resulting zones
-- that would go into an updated scheme.

addForZones: List Segment -> Scheme -> List Zone
addForZones segs scheme =
    let
        newZones = List.map .zone segs
        currentZones = scheme.zones
    in
        Zone.integrate newZones currentZones

-- Given some segments and corresponding zones,
-- work out the corresponding derivations

derivations : List Segment -> List Zone -> List Derivation
derivations segs zones =
    let
        deriv idx seg =
            { cons = Cons.constraint seg zones
            , src = [idx]
            }
    in
        List.indexedMap deriv segs

-- Build a scheme from segments.
-- The base segment will be added at the start, before those given.

scheme : List Segment -> Scheme
scheme segments =
    let
        segments2 = baseSegment :: segments
        newZones = List.map .zone segments2
        zones = Zone.integrate newZones []
        derFn seed derivs = DerivationSet.deriveAllWithLists derivs seed
        seeds = derivations segments2 zones
    in
        { segments = segments2
        , zones = zones
        , derivations = List.foldl derFn [] seeds
        }
