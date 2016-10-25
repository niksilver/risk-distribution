module Derivation exposing
    ( Derivation, derivationToString
    , subtract
    )

import Zone exposing
    ( Zone
    , Change (Subst, Add, NoChange)
    )
import Constraint as Cons exposing (Segment, baseSegment, Constraint)
import Util

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
