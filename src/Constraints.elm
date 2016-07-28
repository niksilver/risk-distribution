module Constraints exposing
    ( inf, Zone, baseZone
    , Relation (Outside, Inside, Edge), relativeTo
    , splitOne
    )

{- How to construct and deduce a distribution.

* Start with a single zone of -inf to +inf at 100%.
  From this we get a constraint A = 100

* Introduce a new judgement of an interval being at a certain %age.
  E.g. "It's 40% likely it's <= 0"

* Use this to split existing zones.
  E.g. Zone A from -inf to +inf is split into
  zone B of -inf to 0 and zone C of 0 to +inf.

* Replace previous variables in constraints with the split variables
  where appropriate.
  E.g. constraint A = 100 becomes B + C = 100

* Add a new constraint matching the new judgement.
  E.g. we add constraint B = 40.

* [X] Find where the LHS of this new constraint is a subset or superset
  of any previous constraint.
  E.g. B is a strict subset of B + C.

* For the new subset/superset pair add a new constraint by subtraction.
  E.g. (B + C = 100) - (B = 40) gives us (C = 60)

* For each new constraint repeat from step [X]. Stop only when there are
  no more new constraints.

* Now we have to find the values of the remaining variables which don't
  have constraints of the form X = [some int]

 -}

inf : Float
inf = 1/0

-- A Zone is a range along the -inf/+inf line which will have some
-- part of the distribution curve.

type alias Zone = { from : Float, to : Float}

baseZone = Zone -inf inf

-- How a value is positioned relative to a zone.

type Relation
    = Inside
    | Outside
    | Edge

-- See where a float is relative to a given zone

relativeTo : Float -> Zone -> Relation
relativeTo x zone =
    if (x < zone.from) then
        Outside
    else if (x > zone.to) then
        Outside
    else if (x == zone.from) then
        Edge
    else if (x == zone.to) then
        Edge
    else
        Inside

-- Take a zone and split it if a given value is inside it (otherwise leave it)

splitOne : Float -> Zone -> List Zone
splitOne x zone =
    case (relativeTo x zone) of
        Inside ->
            [ Zone zone.from x, Zone x zone.to ]
        _ ->
            [ zone ]
