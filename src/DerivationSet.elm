module DerivationSet exposing
    ( empty
    , size, put
    )

import Derivation exposing (Derivation)

import Dict exposing (Dict)


-- A way to store some derivations.
-- Two derivations are equal if their coefficients are equal.
-- We could use a list or a set, but this has simpler operations and
-- is designed to be faster for largers sets.

type alias DerivationSet = Dict String Derivation

toKey : Derivation -> String
toKey deriv =
    toString deriv.cons.coeffs

-- Create an empty set

empty : DerivationSet
empty =
    Dict.empty

-- How many derivations in the set?

size : DerivationSet -> Int
size dSet =
    Dict.size dSet

-- Put a derivation in the set. It will over-write an "equal" one if
-- present

put : Derivation -> DerivationSet -> DerivationSet
put deriv dSet =
    Dict.insert (toKey deriv) deriv dSet
