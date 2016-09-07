module ZoneDict exposing
    ( ZoneDict
    , Value (Exactly, Maximum, Contradiction)
    , getEntries, combine
    , fill, toList
    )

import Zone exposing (Zone)
import Constraint exposing (Constraint)
import Derivation exposing (Derivation)

import Dict exposing (Dict)


-- A Dict where each entry is a Zone, and each value is what we know
-- about that zone's percentage value

type alias ZoneDict =
    Dict (Float, Float) Value

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

-- Fill a ZoneDict based on some zones and the derivations we have about them

fill : List Zone -> List Derivation -> ZoneDict
fill zones derivs =
    fill' zones derivs Dict.empty

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
                Just value1 -> Just (combine value1 value2)
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
