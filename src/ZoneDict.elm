module ZoneDict exposing (ZoneDict, getEntries, fill, toList)

import Zone exposing (inf, Zone)
import Value exposing (Value (Exactly, Maximum, Contradiction))
import Constraint exposing (Constraint)
import Derivation exposing (Derivation)

import Dict exposing (Dict)


-- A Dict where each entry is a Zone, and each value is what we know
-- about that zone's percentage value

type alias ZoneDict =
    Dict (Float, Float) Value


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


-- Fill a ZoneDict based on some zones and the derivations we have about them.
-- Will also rationalise the values

fill : List Zone -> List Derivation -> ZoneDict
fill zones derivs =
    fill' zones derivs Dict.empty
        |> Dict.map (\z v -> Value.rationalise v)

fill' : List Zone -> List Derivation -> ZoneDict -> ZoneDict
fill' zones derivs accum =
    case derivs of
        deriv :: derivs' ->
            fillForDeriv zones deriv accum
                |> fill' zones derivs'
        [] ->
            accum

fillForDeriv : List Zone -> Derivation -> ZoneDict -> ZoneDict
fillForDeriv zones deriv dict =
    let
        entries = getEntries zones deriv
        revise value2 maybeValue1 =
            case maybeValue1 of
                Nothing -> Just value2
                Just value1 -> Just (Value.combine value1 value2)
        update (zone, value) dict1 =
            Dict.update (zone.from, zone.to) (revise value) dict1
    in
        List.foldl update dict entries


-- Get a ZoneDict's entries in the form of a list of pairs

toList : ZoneDict -> List (Zone, Value)
toList dict =
    let
        toZone (from, to) = Zone from to
        convertEntry (pair, value) = (toZone pair, value)
    in
        dict
            |> Dict.toList
            |> List.map convertEntry
