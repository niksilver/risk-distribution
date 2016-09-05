module ZoneDict exposing
    ( Value (Exactly, Maximum)
    , getEntries
    )

import Zone exposing (Zone)
import Constraint exposing (Constraint)
import Derivation exposing (Derivation)


-- Result of successive derivations, showing what we think the
-- value is for a particular zone, and its source(s).

type Value
    = Exactly Int (List Int)
    | Maximum Int (List Int)

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
