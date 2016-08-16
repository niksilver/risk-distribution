module Derivation exposing
    ( Derivation, derivationToString
    )

import Constraints as Con exposing (Constraint)

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
    Con.constraintToString deriv.cons
    ++ " ("
    ++ (deriv.src |> List.map toString |> String.join ", ")
    ++ ")"
