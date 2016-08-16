module Derivation exposing
    ( Derivation, derivationToString
    , subtract
    , deduceOnce
    )

import Constraint as Cons exposing (Constraint)

import String


-- A constraint which also holds the source(s) from which it was derived.

type alias Derivation =
    { cons : Constraint
    , src : List Int
    }

-- The string version of this is just the Constraint plus
-- the source in parentheses

derivationToString : Derivation -> String
derivationToString deriv =
    Cons.constraintToString deriv.cons
    ++ " ("
    ++ (deriv.src |> List.map toString |> String.join ", ")
    ++ ")"

-- Take one derivation and subtract another.
-- The result should include the combined sources

subtract : Derivation -> Derivation -> Derivation
subtract larger smaller =
    Derivation (Cons.subtract larger.cons smaller.cons) (larger.src ++ smaller.src)

-- Deduce more derivations given some existing ones
-- using another to subtract.

deduceOnce : List Derivation -> Derivation -> List Derivation
deduceOnce derivations seed =
    let
        maybeMap d =
            if (seed.cons.coeffs == d.cons.coeffs) then
                Nothing
            else if (Cons.isSubcoeff seed.cons.coeffs d.cons.coeffs) then
                Just (subtract d seed)
            else if (Cons.isSubcoeff d.cons.coeffs seed.cons.coeffs) then
                Just (subtract seed d)
            else
                Nothing
    in
        List.filterMap maybeMap derivations
