module ZoneDict exposing (ZoneDict, Result)

import Zone exposing (Zone)
import Error exposing (Error(Negative, Contradiction))

import Dict exposing (Dict)


type Result
    = XXX

type alias ZoneDict = Dict Zone Result
