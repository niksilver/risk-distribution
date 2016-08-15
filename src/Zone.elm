module Zone exposing
    ( inf
    , Zone, isSubzone
    , Relation (NoRelation, Before, OnEdgeOf, Inside), relativeTo, isInside
    , Change (Subst, Add, NoChange)
    , splitOne, split
    , overlayOnce, overlay
    , apply
    )

import Util

inf : Float
inf = 1/0

-- A Zone is a range along the -inf/+inf line which will have some
-- part of the distribution curve.

type alias Zone = { from : Float, to : Float}

-- See if a zone is within another. A zone is a subzone of itself.

isSubzone : Zone -> Zone -> Bool
isSubzone small large =
    large.from <= small.from
    && small.to <= large.to

-- How a point might relate to the next zone in a sequence
-- We assume the zones are in order.

type Relation
    = NoRelation
    | Before Zone
    | OnEdgeOf Zone
    | Inside Zone

relativeTo : Float -> List Zone -> Relation
relativeTo x zones =
    case zones of
        [] ->
            NoRelation
        head :: tail ->
            if (x < head.from) then
                Before head
            else if (x == head.from) then
                OnEdgeOf head
            else if (x < head.to) then
                Inside head
            else
                relativeTo x tail

-- See if a value is strictly inside a zone

isInside : Float -> Zone -> Bool
isInside x zone =
    (zone.from < x && x < zone.to)

-- Description of a change to a list of zones:
-- We either want to add a zone or substitute a zone.
-- When we split a list of zones we want to know which one (its index)
-- we substitute with which new zones.
-- When we add a zone to a list of zones we want to know where it goes
-- (its index) and what it is.

type Change
    = Subst Int (List Zone)
    | Add Int Zone
    | NoChange

-- Take a zone and split it if a given value is inside it

splitOne : Float -> Zone -> Maybe (List Zone)
splitOne x zone =
    if (isInside x zone) then
        Just [ Zone zone.from x, Zone x zone.to ]
    else
        Nothing

-- Find a split, if there is one, in a list of zones

split : Float -> List Zone -> Change
split x zones =
    split' 0 x zones

split' : Int -> Float -> List Zone -> Change
split' idx x zones =
    case zones of
        [] ->
            NoChange
        zone :: tail ->
            case (splitOne x zone) of
                Nothing ->
                    split' (idx + 1) x tail
                Just new ->
                    Subst idx new

-- Overlay a new zone onto a series of others (which are assumed to be
-- ordered). We return the next one zone change only,
-- and possibly any zone left over.

overlayOnce : Zone -> List Zone -> (Change, Maybe Zone)
overlayOnce zone zones =
    case (relativeTo zone.from zones) of
        NoRelation ->
            overlayOnceNoRelation zone zones
        Before next ->
            overlayOnceBefore zone next zones
        Inside curr ->
            overlayOnceInside zone curr zones
        OnEdgeOf next ->
            overlayOnceOnEdgeOf zone next zones

overlayOnceNoRelation : Zone -> List Zone -> (Change, Maybe Zone)
overlayOnceNoRelation zone zones =
    let
        idx = List.length zones
    in
        (Add idx zone, Nothing)

overlayOnceBefore : Zone -> Zone -> List Zone -> (Change, Maybe Zone)
overlayOnceBefore zone next zones =
    let
        idx = Util.indexOf next zones |> Maybe.withDefault 0
        to' = min zone.to next.from
        zone' = Zone zone.from to'
        remainder =
            if (to' == zone.to) then
                Nothing
            else
                Just (Zone to' zone.to)
    in
        (Add idx zone', remainder)

overlayOnceInside : Zone -> Zone -> List Zone -> (Change, Maybe Zone)
overlayOnceInside zone curr zones =
    let
        idx = Util.indexOf curr zones |> Maybe.withDefault 0
        zone1 = Zone curr.from zone.from
        zone2 = Zone zone.from curr.to
        remainder =
            if (curr.to >= zone.to) then
                Nothing
            else
                Just (Zone curr.to zone.to)
    in
        (Subst idx [zone1, zone2], remainder)

overlayOnceOnEdgeOf : Zone -> Zone -> List Zone -> (Change, Maybe Zone)
overlayOnceOnEdgeOf zone next zones =
    let
        remainder =
            if (zone.to <= next.to) then
                Nothing
            else
                Just (Zone next.to zone.to)
    in
        (NoChange, remainder)

-- Overlay a zone onto a list of other zones, and return what
-- changes need to be made to the original zones.
-- Assumes the other zones are in order.
-- Note that the returned changes have to be used together,
-- because adding one zone or substituting one zone for two
-- others will changes the indexes required of later ones.

overlay : Zone -> List Zone -> List Change
overlay zone zones =
    let
        (revChanges, zones') = overlay' zone zones []
        change = split zone.to zones'
    in
        change :: revChanges
            |> List.filter ((/=) NoChange)
            |> List.reverse

overlay' : Zone -> List Zone -> List Change -> (List Change, List Zone)
overlay' zone zones changes =
    let
        (change, maybeZone) = overlayOnce zone zones
        zones' = apply change zones
        changes' = change :: changes
    in
        case maybeZone of
            Just zone' ->
                overlay' zone' zones' changes'
            Nothing ->
                (changes', zones')

-- Apply a change to some zones

apply : Change -> List Zone -> List Zone
apply change zones =
    case change of
        Add idx new ->
            Util.insert idx [new] zones
        Subst idx new ->
            Util.spliceOne idx new zones
        NoChange ->
            zones
