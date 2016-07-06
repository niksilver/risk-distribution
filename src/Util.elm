module Util exposing
    ( find, findPair
    , sliding
    , bracket
    )


-- See if a list element matches a given criterion

find : (a -> Maybe b) -> List a -> Maybe b
find pred xs =
    case xs of
        [] ->
            Nothing
        head :: tail ->
            case (pred head) of
                Just v -> Just v
                Nothing -> find pred tail

-- See if a pair of elements in a list match a given criterion

findPair : (a -> a -> Maybe b) -> List a -> Maybe b
findPair fn xs =
    let
        findErrorFor x =
            find (fn x) xs
    in
        find findErrorFor xs

-- Groups elements in fixed size blocks by passing a "sliding window" over them.

sliding : Int -> List a -> List (List a)
sliding size elts =
    sliding' size elts []

sliding' : Int -> List a -> List (List a) -> List (List a)
sliding' size elts accum =
    let
        sureTail xs =
            case (List.tail xs) of
                Nothing -> []
                Just tail -> tail
    in
        if (List.length elts >= size) then
            sliding' size (sureTail elts) (List.take size elts :: accum)
        else
            List.reverse accum

-- Bracket a list by repeating the first element at the start and
-- the last element at the end. An empty list remains empty.

bracket : List a -> List a
bracket xs =
    let
        doubleHead ys =
            case ys of
                [] -> []
                head :: tail -> head :: head :: tail
    in
        xs |> List.reverse |> doubleHead |> List.reverse |> doubleHead

