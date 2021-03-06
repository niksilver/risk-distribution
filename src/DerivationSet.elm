module DerivationSet exposing
    ( empty
    , size, put, putList
    , isNew, toReverseList
    , introducesError, skip
    , zeroPcDerivations
    , deriveOnce, deriveAll, deriveAllWithLists
    , lastIsAContradiction
    )

import Constraint as Cons
import Derivation exposing (Derivation)
import Expand

import Dict exposing (Dict)


-- A way to store some derivations.
-- Two derivations are equal if their coefficients are equal.
-- We could use a list or a set, but this has simpler operations and
-- is designed to be faster for largers sets.

type alias DerivationSet =
    { dict : Dict String Derivation
    , list : List Derivation
    }

toKey : Derivation -> String
toKey deriv =
    toString deriv.cons.coeffs

-- Create an empty set

empty : DerivationSet
empty =
    DerivationSet Dict.empty []

-- How many derivations in the set?

size : DerivationSet -> Int
size dSet =
    Dict.size dSet.dict

-- Put a derivation in the set. It will over-write an "equal" one if
-- present

put : Derivation -> DerivationSet -> DerivationSet
put deriv dSet =
    { dict = Dict.insert (toKey deriv) deriv dSet.dict
    , list = deriv :: dSet.list
    }

-- Put many derivations into the set

putList : List Derivation -> DerivationSet -> DerivationSet
putList derivs dSet =
    case derivs of
        [] ->
            dSet
        head :: tail ->
            put head dSet |> putList tail

-- Is the given Derivation new to the set (judging by coeffs only)

isNew : Derivation -> DerivationSet -> Bool
isNew deriv dSet =
    not( Dict.member (toKey deriv) dSet.dict )

-- Get all the derivations out as a list, in reverse order that they went in

toReverseList : DerivationSet -> List Derivation
toReverseList dSet =
    dSet.list

-- Get a Derivation that is equal (same coeffs) as the one given

get : Derivation -> DerivationSet -> Maybe Derivation
get deriv dSet =
    Dict.get (toKey deriv) dSet.dict

-- A Derivation introduces an error to a set if it has a -ve percentage,
-- or if the set contains another Derivation which is the same coeffs but
-- a different percentage

introducesError : Derivation -> DerivationSet -> Bool
introducesError deriv dSet =
    if (deriv.cons.pc < 0) then
        True
    else
        case (get deriv dSet) of
            Nothing ->
                False
            Just der2 ->
                Cons.isContradiction deriv.cons der2.cons

-- True if we skip the given Derivation with this DerivationSet.
-- We should skip it if it's equal to one already there (same coeffs)
-- and not a contradiction

skip : Derivation -> DerivationSet -> Bool
skip deriv dSet =
    case (get deriv dSet) of
        Nothing ->
            False
        Just der2 ->
            deriv.cons.pc == der2.cons.pc

-- For any derivation that has several zones combined to be 0%,
-- create further derivations that tell us each individual zone
-- is at 0%.
-- E.g.
-- Derivation of [ 0, 1, 1, 0 ] at 0% using sources [2, 1] will give
-- Derivation of [ 0, 1, 0, 0 ] at 0% using sources [2, 1] and
-- Derivation of [ 0, 0, 1, 0 ] at 0% using sources [2, 1]

zeroPcDerivations : Derivation -> List Derivation
zeroPcDerivations deriv =
    let
        pc = deriv.cons.pc
        coeffs = deriv.cons.coeffs
        src = deriv.src
        coeffCount = List.sum coeffs
        length = List.length coeffs
        -- Generate coeffs of the form [0, 0, c, 0, 0, 0]
        -- where the c is at a given index
        singletonCoeffs idx coeff =
            List.concat
                [ (List.repeat idx 0)
                , [coeff]
                , (List.repeat (length - idx - 1) 0)
                ]
        ifNonZero coeffs =
            List.sum coeffs /= 0
        restoreDeriv coeffs =
            Derivation { coeffs = coeffs, pc = 0 } src
    in
        if (pc == 0 && coeffCount > 1) then
            (List.indexedMap singletonCoeffs coeffs |> List.filter ifNonZero |> List.map restoreDeriv)
        else
            []

-- Deduce more derivations given some existing ones
-- using another to subtract.

deriveOnce : DerivationSet -> Derivation -> List Derivation
deriveOnce derivations seed =
    let
        derivsList = toReverseList derivations

        maybeMap d =
            if (seed.cons.coeffs == d.cons.coeffs) then
                Nothing
            else if (Cons.isSubcoeff seed.cons.coeffs d.cons.coeffs) then
                Just (Derivation.subtract d seed)
            else if (Cons.isSubcoeff d.cons.coeffs seed.cons.coeffs) then
                Just (Derivation.subtract seed d)
            else
                Nothing

        subtractions =
            List.filterMap maybeMap derivsList

        zeroPcDerivs =
            List.map zeroPcDerivations subtractions |> List.concat
    in
        List.append
            subtractions
            zeroPcDerivs

-- Derive all the derivations we can from some existing ones by
-- adding a new one... including the original ones.
-- We return all the derivations possible, and maybe the erroneous one
-- that caused an error.

deriveAll : DerivationSet -> List Derivation -> (DerivationSet, Maybe Derivation)
deriveAll derivations seeds =
    Expand.expand
        derivations
        seeds
        { skip = (\xs h -> skip h xs)
        , stop = (\xs h -> introducesError h xs)
        , grow = deriveOnce
        , update = (\xs h -> put h xs)
        }

-- Like deriveAll, but instead of a set we provide a list of derivations
-- that go into the set (in order), and we return a correctly-ordered
-- list at the end (and maybe the final problem derivation)

deriveAllWithLists : List Derivation -> List Derivation -> (List Derivation, Maybe Derivation)
deriveAllWithLists derivs seeds =
    let
        dSet = putList derivs empty
        (resultSet, maybeErr) = deriveAll dSet seeds
    in
        (toReverseList resultSet |> List.reverse, maybeErr)


-- See if the last derivation put into a derivation set is a contradiction
-- of any of the others

lastIsAContradiction : DerivationSet -> Bool
lastIsAContradiction dSet =
    case dSet.list of
        [] ->
            False
        dHead :: dTail ->
            List.any (\d -> Cons.isContradiction d.cons dHead.cons) dSet.list
