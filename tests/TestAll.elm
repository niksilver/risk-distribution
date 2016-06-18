import DistributionTest exposing (all)
import FactTest exposing (all)
import ChartTest exposing (all)
import ErrorsTest exposing (all)

import ElmTest exposing (runSuiteHtml, suite)

main : Program Never
main =
    runSuiteHtml <|
        suite "All tests"
        [ DistributionTest.all
        , FactTest.all
        , ChartTest.all
        , ErrorsTest.all
        ]
