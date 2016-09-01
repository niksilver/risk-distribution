module Error exposing
    ( Error (Negative, Contradiction)
    , find
    )

import Zone exposing (Zone)
import Derivation exposing (Derivation, Model)
import Util

import Dict

type Error
    -- Zone(s) together have a negative percentage
    = Negative { zones : List Zone, pc : Int, src : List Int }
    -- Two or more zones have different percentages via different derivations
    | Contradiction { zones : List Zone, pcs : List Int, src : List Int }

-- Find all the errors in a model

find : Model -> List Error
find model =
    List.append
        (findNegative model)
        (findContradiction model)

findNegative : Model -> List Error
findNegative model =
    let
        isNeg deriv =
            deriv.cons.pc < 0
        derivToErr deriv =
            Negative
                { zones = relevantZones model.zones deriv.cons.coeffs
                , pc = deriv.cons.pc
                , src = deriv.src
                }
    in
        model.derivations
            |> List.filter isNeg
            |> List.map derivToErr

relevantZones : List Zone -> List Int -> List Zone
relevantZones zones coeffs =
    List.map2 (,) zones coeffs
        |> List.filter (\zc -> snd zc == 1)
        |> List.map fst

findContradiction : Model -> List Error
findContradiction model =
    let
        -- The equivalence/discriminator between two derivations which
        -- may contradict
        --discriminator : Derivation -> comparable
        discriminator deriv =
            deriv.cons.coeffs |> toString

        -- Useful utility
        lengthTwoOrMore ds =
            List.length ds >= 2

        -- Ensure all derivations in a list have a different percentage,
        -- by removing duplicates
        dedupeByPc : List Derivation -> List Derivation
        dedupeByPc derivs =
            Util.dedupe (\d1 d2 -> d1.cons.pc == d2.cons.pc) derivs

        -- Get groups of derivations that contradict
        getContradictingDerivs : List (List Derivation)
        getContradictingDerivs =
            Util.groupBy discriminator model.derivations
                |> Dict.values
                |> List.map dedupeByPc
                |> List.filter lengthTwoOrMore

        -- We should have some derivations with the same coefficients;
        -- get the coefficients from one of them.
        someCoeffs : List Derivation -> List Int
        someCoeffs derivs =
            derivs
                |> List.head
                |> Maybe.map (.cons >> .coeffs)
                |> Maybe.withDefault []

        -- Convert contradicting derivations to an error
        derivsToErr : List Derivation -> Error
        derivsToErr derivs =
            Contradiction
                { zones = relevantZones model.zones (someCoeffs derivs)
                , pcs = List.map (.cons >> .pc) derivs
                , src = List.map (.src) derivs |> List.concat
                }
    in
        getContradictingDerivs
            |> List.map derivsToErr
