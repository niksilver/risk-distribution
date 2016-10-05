module ZoneDict exposing
    ( ZoneDict
    , Value (Exactly, Maximum, Contradiction), PcValue
    , toValueFloat
    , getEntries, combine, rationalise
    , fill, toList
    )

import Zone exposing (Zone)
import Constraint exposing (Constraint)
import Derivation exposing (Derivation)
import Util

import Dict exposing (Dict)


-- A Dict where each entry is a Zone, and each value is what we know
-- about that zone's percentage value

type alias ZoneDict =
    Dict (Float, Float) PcValue

-- Result of successive derivations, showing what we think the
-- value is for a particular zone, and its source(s).

type Value a
    = Exactly a (List Int)
    | Maximum a (List Int)
    | Contradiction (List Int)

type alias PcValue = Value Int  -- Value for an integer percentage


-- Convert a Value Int to a Value Float

toValueFloat : Value Int -> Value Float
toValueFloat v =
    case v of
        Exactly pc src ->
            Exactly (toFloat pc) src
        Maximum pc src ->
            Maximum (toFloat pc) src
        Contradiction src ->
            Contradiction src

-- Get some entries for a ZoneDict.
-- Given some zones and a derivation each entry shows the best result
-- for each zone expressed in the constraint.

getEntries : List Zone -> Derivation -> List (Zone, PcValue)
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

combine : PcValue -> PcValue -> PcValue
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

-- Convert "Maximum 0" to "Exactly 0", and remove duplicate sources,
-- otherwise keep the same thing.

rationalise : PcValue -> PcValue
rationalise v =
    case v of
        Exactly a src ->
            Exactly a (Util.dedupe (==) src)
        Maximum 0 src ->
            Exactly 0 (Util.dedupe (==) src)
        Maximum a src ->
            Maximum a (Util.dedupe (==) src)
        Contradiction src ->
            Contradiction (Util.dedupe (==) src)

-- Fill a ZoneDict based on some zones and the derivations we have about them.
-- Will also rationalise the values

fill : List Zone -> List Derivation -> ZoneDict
fill zones derivs =
    fill' zones derivs Dict.empty
        |> Dict.map (\z v -> rationalise v)

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

toList : ZoneDict -> List (Zone, PcValue)
toList dict =
    let
        toZone (from, to) = Zone from to
        convertEntry (pair, value) = (toZone pair, value)
    in
        dict
            |> Dict.toList
            |> List.map convertEntry
