module DerivationScheme exposing
    ( Scheme
    , derivations
    , scheme
    )

import Zone exposing (Zone)
import Segment exposing (Segment)
import Derivation exposing (Derivation)
import DerivationSet


-- A scheme represents:
-- our segments (our judgements, or claims, over the distribution),
-- the zones, and
-- the derivations (with maybe the error-causing derivation)


type alias Scheme =
    { segments : List Segment
    , zones : List Zone
    , derivations : (List Derivation, Maybe Derivation)
    }


-- Given some segments and corresponding zones,
-- work out the corresponding derivations

derivations : List Segment -> List Zone -> List Derivation
derivations segs zones =
    let
        deriv idx seg =
            { cons = Segment.constraint seg zones
            , src = [idx]
            }
    in
        List.indexedMap deriv segs

-- Build a scheme from segments.
-- The base segment will be added at the start, before those given.

scheme : List Segment -> Scheme
scheme segments =
    let
        segments2 = Segment.base :: segments
        newZones = List.map .zone segments2
        zones = Zone.integrate newZones []
        seeds = derivations segments2 zones
        derivs = DerivationSet.deriveAllWithLists [] seeds
    in
        { segments = segments2
        , zones = zones
        , derivations = derivs
        }
