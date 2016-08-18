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
        |> Maybe.map Util.singleton
        |> Maybe.withDefault []

findNegative : Model -> Maybe Error
findNegative model =
    let
        getNeg deriv =
            if (deriv.cons.pc < 0) then Just deriv else Nothing
        negDeriv =
            Util.find getNeg model.derivations
        derivToErr deriv =
            Negative
                { zones = relevantZones model.zones deriv.cons.coeffs
                , pc = deriv.cons.pc
                , src = deriv.src
                }
    in
        Maybe.map derivToErr negDeriv

relevantZones : List Zone -> List Int -> List Zone
relevantZones zones coeffs =
    List.map2 (,) zones coeffs
        |> List.filter (\zc -> snd zc == 1)
        |> List.map fst
