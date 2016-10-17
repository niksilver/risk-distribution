import DistributionTest exposing (all)
import ZoneTest exposing (all)
import ConstraintTest exposing (all)
import DerivationTest exposing (all)
import ErrorTest exposing (all)
import ValueTest exposing (all)
import ZoneDictTest exposing (all)
import BlockTest exposing (all)
import FactTest exposing (all)
import SpecTest exposing (all)
-- import ChartTest exposing (all) -- Needs to be restored!
import UtilTest exposing (all)
import AxisTest exposing (all)
import PathTest exposing (all)
import SplineTest exposing (all)

import ElmTest exposing (runSuiteHtml, suite)

main : Program Never
main =
    runSuiteHtml <|
        suite "All tests"
        [ DistributionTest.all
        , ZoneTest.all
        , ConstraintTest.all
        , DerivationTest.all
        , ErrorTest.all
        , ValueTest.all
        , ZoneDictTest.all
        , BlockTest.all
        , FactTest.all
        , SpecTest.all
        -- , ChartTest.all -- Needs to be restored!
        , UtilTest.all
        , AxisTest.all
        , PathTest.all
        , SplineTest.all
        ]
