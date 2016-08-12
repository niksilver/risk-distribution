module Util exposing
    ( singleton
    , find, findPair
    , sliding
    , bracket, bracketMap
    , spliceOne, insert
    , nth, indexOf
    , expand
    , toLetter
    )

import String


-- Create a singleton list of the given elements

singleton : a -> List a
singleton x =
    [x]

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

-- Insert some elements at an index in a list.
-- If the index is out of bounds there's no effect.

insert : Int -> List a -> List a -> List a
insert idx new orig =
    let
        before = List.take idx orig
        after = List.drop idx orig
    in
        if (0 <= idx && idx <= List.length orig) then
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

-- Find where an element is in a list

indexOf : a -> List a -> Maybe Int
indexOf x xs =
    indexOf' x xs 0

indexOf' : a -> List a -> Int -> Maybe Int
indexOf' x xs idx =
    case xs of
        [] ->
            Nothing
        head :: tail ->
            if (x == head) then
                Just idx
            else
                indexOf' x tail (idx + 1)

-- Expand a list as follows...
-- A function fn takes a list of elements and new element a, and returns a
-- new list.
-- We take a seed, and imagine it is at the head of a queue (actually, the
-- only element in the queue, currently).
-- We filter the queue so that it contains only elements that aren't already
-- in the list.
-- We apply fn to the original list and the head of the queue.
-- We put the returned list at the back of the queue.
-- We put the head of the queue (the seed) onto the end of the original
-- list. Then we repeat the process from the filtering step
-- with the updated list and the updated head of the queue.
-- We repeat from the filtering step until the queue is empty.
-- This function may not terminate if you're not careful.
--
-- Example:
-- fn multiplies all elements of the list by the given element, does modulo 10.
-- Start with list [2, 3] and seed 4.
-- The queue is [4] and filtering again gives [4].
-- The result of fn is [2*4 mod 10, 3*4 mod 8] = [8, 2].
-- We put 4 onto the end of the list to give [2, 3, 4].
-- We filter the queue to give [8] (because 2 is already in the list).
-- We apply fn to the list and [8], which gives
-- [2*8 mod 10, 3*8 mod 10, 4*8 mod 10] = [6, 4, 2].
-- We put the seed onto the end to give [2, 3, 4, 8].
-- Filtering the queue gives [6].
-- So our new seed is 6.
-- Applying fn gives result
-- [2*6 mod 10, 3*6 mod 10, 4*6 mod 10, 8*6 mod 10] = [2, 8, 4, 8].
-- We put the seed on the end of the list to give [2, 3, 4, 8, 6].
-- We filter the queue to give [].
-- And now we're done: the result is [2, 3, 4, 8, 6].

expand : (List a -> a -> List a) -> List a -> a -> List a
expand fn xs seed =
    expand' fn xs [seed]

expand' : (List a -> a -> List a) -> List a -> List a -> List a
expand' fn xs queue =
    let
        notInList e = not (List.member e xs)
        filtQueue = List.filter notInList queue
    in
        case filtQueue of
            [] ->
                xs
            seed :: tail ->
                let
                    tail2 = fn xs seed
                    xs2 = List.append xs [seed]
                    queue2 = List.append tail tail2
                in
                    expand' fn xs2 queue2
-- Convert an Int 0 25 to a lower case letter

toLetter : Int -> String
toLetter idx =
    let
        az = "abcdefghijklmnopqrstuvwxyz"
    in
        if (0 <= idx && idx < String.length az) then
            String.slice idx (idx+1) az
        else
            "?"
