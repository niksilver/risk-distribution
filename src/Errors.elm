module Errors exposing (Error (MoreThan100Percent), errors)

import Distribution as Dist exposing (Layer, Limit (AtLeast, AtMost))

-- Kinds of error

type Error
    = MoreThan100Percent

-- List any errors found given some layers

errors : List Layer -> List Error
errors ys =
    List.concat
    [ moreThan100PercentErrors ys
    ]

exists : (a -> Bool) -> List a -> Bool
exists pred xs =
    case xs of
        [] ->
            False
        head :: tail ->
            if (pred head) then True else exists pred tail

moreThan100PercentErrors : List Layer -> List Error
moreThan100PercentErrors ys =
    let
        divergent y1 y2 =
            (y1.limit == AtMost && y2.limit == AtLeast && y1.value < y2.value)
        over100Pc y1 y2 =
            (y1.value + y2.value > 1.00)
        isErrorCounterpart y1 y2 =
            (divergent y1 y2) && (over100Pc y1 y2)
        hasErrorCounterpart y1 =
            exists (isErrorCounterpart y1) ys
    in
        if (exists hasErrorCounterpart ys) then
            [ MoreThan100Percent ]
        else
            []

