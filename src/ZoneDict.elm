module ZoneDict exposing
    ( Value (Exactly, Maximum, Contradiction)
    , getEntries, combine
    )

import Zone exposing (Zone)
import Constraint exposing (Constraint)
import Derivation exposing (Derivation)


-- Result of successive derivations, showing what we think the
-- value is for a particular zone, and its source(s).

type Value
    = Exactly Int (List Int)
    | Maximum Int (List Int)
    | Contradiction (List Int)

-- Get some entries for a ZoneDict.
-- Given some zones and a derivation each entry shows the best result
-- for each zone expressed in the constraint.

getEntries : List Zone -> Derivation -> List (Zone, Value)
getEntries zones {cons, src} =
    let
        includeZone zone coeff =
            if (coeff == 0) then Nothing else Just zone
        value =
            if (List.sum cons.coeffs == 1) then
                Exactly cons.pc src
            else
                Maximum cons.pc src
    in
        List.map2 includeZone zones cons.coeffs
            |> List.filterMap identity
            |> List.map (\z -> (z, value))

-- Combine one value with another to see if there is consistency or otherwise
-- a better understanding of that particular zone.

combine : Value -> Value -> Value
combine v1 v2 =
    case (v1, v2) of
        (Contradiction src1, Contradiction src2) ->
            Contradiction (List.append src1 src2)
        (Contradiction _, _) ->
            v1
        (_, Contradiction _) ->
            v2
        (Exactly pc1 src1, Exactly pc2 src2) ->
            if (pc1 == pc2) then
                v1
            else
                Contradiction (List.append src1 src2)
        (Exactly pc1 src1, Maximum pc2 src2) ->
            if (pc1 <= pc2) then
                v1
            else
                Contradiction (List.append src1 src2)
        (Maximum pc1 src1, Exactly pc2 src2) ->
            if (pc1 >= pc2) then
                v2
            else
                Contradiction (List.append src1 src2)
        (Maximum pc1 src1, Maximum pc2 src2) ->
            if (pc1 <= pc2) then
                v1
            else
                v2
