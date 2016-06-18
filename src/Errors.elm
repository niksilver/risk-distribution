module Errors exposing
    ( Error (MoreThan100Percent), IndexedLayer
    , index, errors
    , view
    )

import Distribution as Dist exposing (Layer, Limit (AtLeast, AtMost))

import Html exposing (Html, ul, li, text)


-- Kinds of error

type Error
    = MoreThan100Percent Int Int  -- The index of the offending layers

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

-- Utility: Find the first successful test result from a list

find : (a -> Maybe b) -> List a -> Maybe b
find pred xs =
    case xs of
        [] ->
            Nothing
        head :: tail ->
            case (pred head) of
                Just v -> Just v
                Nothing -> find pred tail

-- List any errors found given some layers

errors : List Layer -> List Error
errors ys =
    List.concat
    [ moreThan100PercentErrors ys
    ]

moreThan100PercentErrors : List Layer -> List Error
moreThan100PercentErrors ys =
    let
        iys = index ys
        divergent y1 y2 =
            (y1.limit == AtMost && y2.limit == AtLeast && y1.value <= y2.value)
        over100Pc y1 y2 =
            (y1.value + y2.value > 1.00)
        getError iy1 iy2 =
            if (divergent iy1.layer iy2.layer) && (over100Pc iy1.layer iy2.layer)
            then
                Just (MoreThan100Percent iy1.index iy2.index) |> Debug.log "just"
            else
                Nothing
        findErrorFor iy1 =
            find (getError iy1) iys
    in
        case (find findErrorFor iys) of
            Just err -> [ err ]
            Nothing -> []

-- View of errors

view : List Error -> Html x
view errs =
    ul [] (List.map viewOneError errs)

viewOneError : Error -> Html x
viewOneError err =
    let
        (i1', i2') =
            case err of
                MoreThan100Percent i1 i2 ->
                    if i1 < i2 then (i1, i2) else (i2, i1)
        message =
                "Lines " ++ (i1' + 1 |> toString)
                ++ " and " ++ (i2' + 1 |> toString)
                ++ " suggest a space of more than 100%"
    in
        li [] [ text message ]

