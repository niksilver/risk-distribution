module Error exposing
    ( Error (Negative)
    , find
    )

import Zone exposing (Zone)
import Derivation exposing (Model)
import Util

type Error =
    Negative { zones : List Zone, pc : Int, src : List Int }

-- Find all the errors in a model

find : Model -> List Error
find model =
    findNegative model

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
