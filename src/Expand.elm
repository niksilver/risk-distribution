module Expand exposing
    ( expand, skipWhile
    )

-- Expand something in a controlled manner. We go in with
--   - Data of type a that we want to expand.
--   - A list of things of type b that are used to expand it.
--   - A skip function that decides if it should skip the item at the
--     head of the queue.
--   - A stop function that considers whether to stop the expansion.
--   - A grow function that adds more items to the back of the queue.
--   - An update function that expands the data.
-- All the functions operate on the data and the head of the queue.
-- The algorithm is as follows:
--   - Keep skipping the item at the head of the queue. If the queue empties
--     then return just the data (type a) and Nothing of type b.
--   - Take the item from the head of the queue.
--   - Use the stop test on it to see if we should stop now. If so, we
--     return the data and Just the item which caused the stop. Otherwise...
--   - Use the grow function to add items to the back of the queue.
--   - Update the data.
--   - Repeat from the skipping step.

type alias Expanders a b =
    { skip : a -> b -> Bool
    , stop : a -> b -> Bool
    , grow : a -> b -> List b
    , update : a -> b -> a
    }

expand : a -> List b -> Expanders a b -> (a, Maybe b)
expand data queue fns =
    let
        queue' = skipWhile (fns.skip data) queue
    in
        case queue' of
            [] ->
                (data, Nothing)
            head :: tail ->
                if (fns.stop data head) then
                    (data, Just head)
                else
                    let
                        queueEnd = fns.grow data head
                        queue2 = List.append tail queueEnd
                        data2 = fns.update data head
                    in
                        expand data2 queue2 fns

-- Keep skipping items at the front of the queue until we're told not to
-- (or the list is empty)

skipWhile : (b -> Bool) -> List b -> List b
skipWhile skipFn queue =
    case queue of
        [] ->
            []
        qHead :: qTail ->
            if (skipFn qHead) then
                skipWhile skipFn qTail
            else
                queue
