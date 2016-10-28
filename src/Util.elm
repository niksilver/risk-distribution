module Util exposing
    ( isFinite
    , singleton
    , none
    , find
    , groupBy
    , sliding
    , bracket, bracketMap
    , spliceOne, insert
    , ith, indexOf
    , dedupe
    , filteredExpand
    , toLetter
    )

import String
import Dict exposing (Dict)


-- Is a float finite?

isFinite : Float -> Bool
isFinite x =
    (not <| isInfinite x) && (not <| isNaN x)

-- Create a singleton list of the given elements

singleton : a -> List a
singleton x =
    [x]

-- True if none of a list matches a predicate

none : (a -> Bool) -> List a -> Bool
none pred xs =
    List.any pred xs |> not

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

-- Group elements by a discriminator function.
-- E.g. grouping [54, 6, 14] by "last digit" will give
-- a Dict of 4 -> [54, 14], and 6 -> [6].

groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy discr xs =
    let
        rev k xs = List.reverse xs
    in
        groupBy' discr xs Dict.empty
            |> Dict.map rev

groupBy' : (a -> comparable) -> List a -> Dict comparable (List a) -> Dict comparable (List a)
groupBy' discr xs accum =
    let
        groupWith v mv =
            case mv of
                Nothing -> Just [v]
                Just vs -> Just (v :: vs)
        augment v =
            Dict.update (discr v) (groupWith v) accum
    in
        case xs of
            [] ->
                accum
            head :: tail ->
                groupBy' discr tail (augment head)

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

-- Find the ith item in a list, where 0 is for the first element

ith : Int -> List a -> Maybe a
ith i xs =
    if (i == 0) then
        List.head xs
    else
        case List.tail xs of
            Nothing -> Nothing
            Just tail -> ith (i-1) tail

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

-- Deduplicate a list by removing elements that are equivalent
-- to earlier elements.

dedupe : (a -> a -> Bool) -> List a -> List a
dedupe equiv xs =
    dedupe' equiv xs []
        |> List.reverse

dedupe' : (a -> a -> Bool) -> List a -> List a -> List a
dedupe' equiv xs accum =
    case xs of
        [] ->
            accum
        head :: tail ->
            if (List.any (equiv head) accum) then
                dedupe' equiv tail accum
            else
                dedupe' equiv tail (head :: accum)

-- Expand a list as follows...
-- We start with:
--   - A function fn takes a list of elements and new element a, and returns a
--     new list.
--   - A filter function, pred, allows an element only if it passes a test based on
--     the contents of a list.
--   - An initial list
--   - A seed
-- We proceed like this:
--   - Initialise the queue. Create a singleton list with just the seed.
--   - Filter the queue's head:
--     - Drop the head repeatedly until it passes the pred function.
--   - Extend the queue:
--     - Apply fn to the original list and the head of the queue.
--     - Put the returned list at the back of the queue.
--   - Put the head of the queue onto the end of the original list.
--   - Repeat the process from the filtering step
-- Warning: This function will not terminate if the queue never becomes [].
--
-- Example:
-- fn multiplies all elements of the list by the given element,
-- and pred says it will allow an element its last digit is unique among the
-- given list (i.e. if there's nothing there equal modulo 10).
-- Start with list [2, 3] and seed 4.
-- The queue is [4] and filtering again gives [4].
-- The result of fn is [2*4, 3*4] = [8, 12].
-- We put 4 onto the end of the list to give [2, 3, 4].
-- We filter the queue to give [8] (because only 8 has a unique last digit, not 12).
-- We apply fn to the list and [8], which gives
-- [2*8, 3*8, 4*8] = [16, 24, 32].
-- We put the seed onto the end to give [2, 3, 4, 8].
-- Filtering the queue gives [16].
-- So our new seed is 16.
-- Applying fn gives result
-- [2*16, 3*16, 4*16, 8*16] = [32, 48, 64, 128].
-- We put the seed on the end of the list to give [2, 3, 4, 8, 16].
-- We filter the queue to give [].
-- And now we're done: the result is [2, 3, 4, 8, 16].

filteredExpand : (List a -> a -> List a) -> (List a -> a -> Bool)-> List a -> a -> List a
filteredExpand fn pred xs seed =
    filteredExpand' fn pred xs [seed]

filteredExpand' : (List a -> a -> List a) -> (List a -> a -> Bool) -> List a -> List a -> List a
filteredExpand' fn pred xs queue =
    let
        dropUntil q2 =
            case q2 of
                [] ->
                    []
                qHead :: qTail ->
                    if (pred xs qHead) then
                        q2
                    else
                        dropUntil qTail
        filtQueue = dropUntil queue
        -- q x = List.length x |> toString
    in
        case filtQueue of
            [] ->
                xs
            seed :: tail ->
                let
                    tail2 = fn xs seed
                    xs2 = List.append xs [seed]
                    queue2 = List.append tail tail2
                    -- x =
                    --     if List.length xs2 > 50 then
                    --         Debug.log ("List " ++ q xs2 ++ ", q before " ++ q queue ++ ", q after " ++ q queue2) ""
                    --     else
                    --         ""
                in
                    filteredExpand' fn pred xs2 queue2

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
