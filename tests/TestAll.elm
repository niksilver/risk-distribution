import ZoneTest exposing (all)
import ConstraintTest exposing (all)
import SegmentTest exposing (all)
import DerivationTest exposing (all)
import ErrorTest exposing (all)
import ValueTest exposing (all)
import ZoneDictTest exposing (all)
import BlockTest exposing (all)
import FactTest exposing (all)
import FactListTest exposing (all)
import SpecTest exposing (all)
import UtilTest exposing (all)
import ExpandTest exposing (all)
import DerivationSetTest exposing (all)
import DerivationSchemeTest exposing (all)
import AxisTest exposing (all)
import PathTest exposing (all)
import SplineTest exposing (all)

import ElmTest exposing (runSuiteHtml, suite)

main : Program Never
main =
    runSuiteHtml <|
        suite "All tests"
        [ ZoneTest.all
        , ConstraintTest.all
        , SegmentTest.all
        , DerivationTest.all
        , ErrorTest.all
        , ValueTest.all
        , ZoneDictTest.all
        , BlockTest.all
        , FactTest.all
        , FactListTest.all
        , SpecTest.all
        , UtilTest.all
        , ExpandTest.all
        , DerivationSetTest.all
        , DerivationSchemeTest.all
        , AxisTest.all
        , PathTest.all
        , SplineTest.all
        ]
