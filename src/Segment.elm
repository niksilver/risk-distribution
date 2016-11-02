module Segment exposing (Segment, baseSegment, constraint)

import Constraint exposing (Constraint)
import Zone exposing (Zone, inf)


-- A segemnt of our distribution representing the idea that, say,
-- '40% of the distribution lies between -10 and +10'

type alias Segment =
    { pc : Int
    , zone : Zone
    }


-- A segment saying everything must add up to 100%

baseSegment : Segment
baseSegment =
    Segment 100 (Zone -inf inf)

-- Create a Constraint describing a segment, and given our total list
-- of zones

constraint : Segment -> List Zone -> Constraint
constraint seg zones =
    let
        coeff zone =
            if (Zone.isSubzone zone seg.zone) then 1 else 0
    in
        Constraint (List.map coeff zones) seg.pc
