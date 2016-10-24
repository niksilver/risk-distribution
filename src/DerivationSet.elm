module DerivationSet exposing
    ( empty
    , size, put
    , isNew, toReverseList
    , isContradiction, skip
    )

import Constraint as Cons
import Derivation exposing (Derivation)

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

-- A Derivation is a contradiction to a set if the set contains another
-- Derivation which is the same coeffs but a different percentage

isContradiction : Derivation -> DerivationSet -> Bool
isContradiction deriv dSet =
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
