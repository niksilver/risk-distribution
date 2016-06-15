module Errors exposing (errors)

import Distribution as Dist exposing (Layer)

-- Kinds of error

type Error
    = MoreThan100Percent

-- List any errors found given some layers

errors : List Layer -> List Error
errors ys =
    []

