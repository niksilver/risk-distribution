module ZoneDict exposing
    ( Value (Exactly, Maximum)
    , getEntries
    )

import Zone exposing (Zone)
import Constraint exposing (Constraint)


-- Result of successive derivations, showing what we think the
-- value is for a particular zone.

type Value
    = Exactly Int
    | Maximum Int

-- Get some entries for a ZoneDict.
-- Given some zones and a constraint each entry shows the best result
-- for each zone expressed in the constraint.

getEntries : List Zone -> Constraint -> List (Zone, Value)
getEntries zones cons =
    let
        includeZone zone coeff =
            if (coeff == 0) then Nothing else Just zone
        value =
            if (List.sum cons.coeffs == 1) then
                Exactly cons.pc
            else
                Maximum cons.pc
    in
        List.map2 includeZone zones cons.coeffs
            |> List.filterMap identity
            |> List.map (\z -> (z, value))
