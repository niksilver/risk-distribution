module Util exposing (find, findPair)

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

