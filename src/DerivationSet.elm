module DerivationSet exposing
    ( empty
    , size, put
    )

import Derivation exposing (Derivation)

import DictSet exposing (DictSet)


-- A way to store some derivations.
-- Two derivations are equal if their coefficients are equal.
-- We could use a list or a set, but this has simpler operations and
-- is designed to be faster for largers sets.

type alias DerivationSet = DictSet String Derivation


-- Create an empty set

empty : DerivationSet
empty =
    DictSet.empty (\deriv -> deriv.cons.coeffs |> toString)

-- How many derivations in the set?

size : DerivationSet -> Int
size dSet =
    DictSet.size dSet

-- Put a derivation in the set. It will over-write an "equal" one if
-- present

put : Derivation -> DerivationSet -> DerivationSet
put deriv dSet =
    DictSet.insert deriv dSet
