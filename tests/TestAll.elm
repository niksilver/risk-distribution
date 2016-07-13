import DistributionTest exposing (all)
import FactTest exposing (all)
import ChartUtilTest exposing (all)
import ChartTest exposing (all)
import ErrorsTest exposing (all)
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
        , FactTest.all
        , ChartUtilTest.all
        , ChartTest.all
        , ErrorsTest.all
        , UtilTest.all
        , AxisTest.all
        , PathTest.all
        , SplineTest.all
        ]
