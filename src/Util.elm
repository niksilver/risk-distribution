module Util exposing
    ( find, findPair
    , sliding
    , bracket, bracketMap
    , spliceOne
    , nth
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
    bracketMap identity identity xs

-- Bracket a list by prepending the first element with a function applied
-- and appending the last element with a function applied.

bracketMap : (a -> a) -> (a -> a) -> List a -> List a
bracketMap fnFront fnBack xs =
    bracketMap' fnFront xs
        |> List.reverse
        |> bracketMap' fnBack
        |> List.reverse

bracketMap' : (a -> a) -> List a -> List a
bracketMap' fn xs =
    case xs of
        [] ->
            []
        head :: tail ->
            (fn head) :: xs

-- Take a list, and splice in another, replacing one element at
-- the given index

spliceOne : Int -> List a -> List a -> List a
spliceOne idx new orig =
    let
        before = List.take idx orig
        after = List.drop (idx + 1) orig
    in
        if (0 <= idx && idx < List.length orig) then
            List.concat [ before, new, after ]
        else
            orig

-- Find the nth item in a list, where 0 is for the first element

nth : Int -> List a -> Maybe a
nth i xs =
    if (i == 0) then
        List.head xs
    else
        case List.tail xs of
            Nothing -> Nothing
            Just tail -> nth (i-1) tail
    
