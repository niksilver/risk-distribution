module Errors exposing
    ( Error
        ( MoreThan100Percent
        , NoUpperLimit
        , NoLowerLimit
        , Contradiction
        , CantMake100Percent
        )
    , IndexedLayer
    , index, errors
    , view
    )

import Distribution as Dist exposing (Layer, Limit (AtLeast, AtMost))
import Util exposing (findPair)

import Html exposing (Html, ul, li, text)


-- Kinds of error

type Error
    = MoreThan100Percent Int Int  -- The index of the offending layers
    | NoUpperLimit
    | NoLowerLimit
    | Contradiction Int Int  -- Index of the contradicting layers
    | CantMake100Percent Int Int  -- The index of the offending layers

type alias Indexed a
    = { a | index : Int }

type alias IndexedLayer
    = { layer : Layer, index : Int }


-- Apply indexes to layers

index : List Layer -> List IndexedLayer
index ys =
    let
        index' idx y = { layer = y, index = idx }
    in
        List.indexedMap index' ys

-- List any errors found given some layers

errors : List Layer -> List Error
errors ys =
    List.concat
    [ moreThan100PercentError ys
    , noUpperLimitError ys
    , noLowerLimitError ys
    , contradictionError ys
    , cantMake100PercentError ys
    ]

moreThan100PercentError : List Layer -> List Error
moreThan100PercentError ys =
    let
        iys = index ys
        divergent y1 y2 =
            (y1.limit == AtMost && y2.limit == AtLeast && y1.value <= y2.value)
        over100Pc y1 y2 =
            (y1.prob + y2.prob > 1.00)
        getError iy1 iy2 =
            if (divergent iy1.layer iy2.layer) && (over100Pc iy1.layer iy2.layer)
            then
                Just (MoreThan100Percent iy1.index iy2.index)
            else
                Nothing
    in
        case (findPair getError iys) of
            Just err -> [ err ]
            Nothing -> []

noUpperLimitError : List Layer -> List Error
noUpperLimitError ys =
    if (List.all (\y -> y.limit == AtLeast) ys) then
        [ NoUpperLimit ]
    else
        []

noLowerLimitError : List Layer -> List Error
noLowerLimitError ys =
    if (List.all (\y -> y.limit == AtMost) ys) then
        [ NoLowerLimit ]
    else
        []

contradictionError : List Layer -> List Error
contradictionError ys =
    let
        iys = index ys
        ordered y1 y2 = (y1.value < y2.value)
        bothAtLeast y1 y2 = (y1.limit == AtLeast && y2.limit == AtLeast)
        bothAtMost y1 y2 = (y1.limit == AtMost && y2.limit == AtMost)
        wrongAtLeast y1 y2 = (y1.prob < y2.prob)
        wrongAtMost y1 y2 = (y1.prob > y2.prob)
        contradiction iy1 iy2 =
            let
                y1 = iy1.layer
                y2 = iy2.layer
            in
                if (ordered y1 y2
                    && (  (bothAtLeast y1 y2 && wrongAtLeast y1 y2)
                       || (bothAtMost y1 y2 && wrongAtMost y1 y2)
                       )
                   )
                then
                    Just (iy1.index, iy2.index)
                else
                    Nothing
    in
        case (findPair contradiction iys) of
            Just (i1, i2) -> [ Contradiction i1 i2 ]
            Nothing -> []

cantMake100PercentError : List Layer -> List Error
cantMake100PercentError ys =
    let
        iys = index ys
        orderedOrEq y1 y2 = (y1.value <= y2.value)
        overlapping y1 y2 = (y1.limit == AtLeast && y2.limit == AtMost)
        tooSmall y1 y2 = (y1.prob + y2.prob < 1.00)
        getError iy1 iy2 =
            let
                y1 = iy1.layer
                y2 = iy2.layer
            in
                if (orderedOrEq y1 y2
                    && overlapping y1 y2
                    && tooSmall y1 y2)
                then
                    Just (iy1.index, iy2.index)
                else
                    Nothing
    in
        case (findPair getError iys) of
            Just (i1, i2) -> [ CantMake100Percent i1 i2 ]
            Nothing -> []

-- View of errors

view : List Error -> Html x
view errs =
    ul [] (List.map viewOneError errs)

viewOneError : Error -> Html x
viewOneError err =
    let
        message =
            case err of
                MoreThan100Percent i1 i2 ->
                    moreThan100PercentMessage i1 i2
                NoUpperLimit ->
                    "You've not given an upper limit for the value"
                NoLowerLimit ->
                    "You've not given a lower limit for the value"
                Contradiction i1 i2 ->
                    contradictionMessage i1 i2
                CantMake100Percent i1 i2 ->
                    "Not implemented!"
    in
        li [] [ text message ]

moreThan100PercentMessage : Int -> Int -> String
moreThan100PercentMessage i1 i2 =
    let
        (i1', i2') =
            if i1 < i2 then (i1, i2) else (i2, i1)
     in
        "Lines " ++ (i1' + 1 |> toString)
        ++ " and " ++ (i2' + 1 |> toString)
        ++ " suggest a space of more than 100%"

contradictionMessage : Int -> Int -> String
contradictionMessage i1 i2 =
    let
        (i1', i2') =
            if i1 < i2 then (i1, i2) else (i2, i1)
     in
        "Lines " ++ (i1' + 1 |> toString)
        ++ " and " ++ (i2' + 1 |> toString)
        ++ " contradict each other"

