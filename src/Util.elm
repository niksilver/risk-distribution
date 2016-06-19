module Util exposing (findPair)

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

findPair : (a -> a -> Maybe b) -> List a -> Maybe b
findPair fn iys =
    let
        findErrorFor iy1 =
            find (fn iy1) iys
    in
        find findErrorFor iys

