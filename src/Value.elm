module Value exposing
    ( Value (Exactly, Maximum, Contradiction)
    , percent, combine, rationalise
    )

import Util


-- Result of successive derivations, showing what we think the
-- value is for a particular zone, and its source(s).

type Value
    = Exactly Int (List Int)
    | Maximum Int (List Int)
    | Contradiction (List Int)


-- Get the percent size of a Value

percent : Value -> Maybe Int
percent value =
    case value of
        Exactly p src -> Just p
        Maximum p src -> Just p
        Contradiction src -> Nothing


-- Combine one value with another to see if there is consistency or otherwise
-- a better understanding of that particular zone.

combine : Value -> Value -> Value
combine v1 v2 =
    case (v1, v2) of
        (Contradiction src1, Contradiction src2) ->
            Contradiction (List.append src1 src2)
        (Contradiction _, _) ->
            v1
        (_, Contradiction _) ->
            v2
        (Exactly pc1 src1, Exactly pc2 src2) ->
            if (pc1 == pc2) then
                v1
            else
                Contradiction (List.append src1 src2)
        (Exactly pc1 src1, Maximum pc2 src2) ->
            if (pc1 <= pc2) then
                v1
            else
                Contradiction (List.append src1 src2)
        (Maximum pc1 src1, Exactly pc2 src2) ->
            if (pc1 >= pc2) then
                v2
            else
                Contradiction (List.append src1 src2)
        (Maximum pc1 src1, Maximum pc2 src2) ->
            if (pc1 <= pc2) then
                v1
            else
                v2

-- Convert "Maximum 0" to "Exactly 0", and remove duplicate sources,
-- otherwise keep the same thing.

rationalise : Value -> Value
rationalise v =
    case v of
        Exactly a src ->
            Exactly a (Util.dedupe (==) src)
        Maximum 0 src ->
            Exactly 0 (Util.dedupe (==) src)
        Maximum a src ->
            Maximum a (Util.dedupe (==) src)
        Contradiction src ->
            Contradiction (Util.dedupe (==) src)
