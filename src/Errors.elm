module Errors exposing
    ( Error (MoreThan100Percent, NoUpperLimit, NoLowerLimit)
    , IndexedLayer
    , index, errors
    , view
    )

import Distribution as Dist exposing (Layer, Limit (AtLeast, AtMost))

import Html exposing (Html, ul, li, text)


-- Kinds of error

type Error
    = MoreThan100Percent Int Int  -- The index of the offending layers
    | NoUpperLimit
    | NoLowerLimit

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
    [ moreThan100PercentErrors ys
    , noUpperLimitError ys
    , noLowerLimitError ys
    ]

find : (a -> Maybe b) -> List a -> Maybe b
find pred xs =
    case xs of
        [] ->
            Nothing
        head :: tail ->
            case (pred head) of
                Just v -> Just v
                Nothing -> find pred tail

moreThan100PercentErrors : List Layer -> List Error
moreThan100PercentErrors ys =
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
        findErrorFor iy1 =
            find (getError iy1) iys
    in
        case (find findErrorFor iys) of
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
