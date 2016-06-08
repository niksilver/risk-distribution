module Chart exposing (view)

import FactList
import Distribution as Dist

import Html exposing (Html)
import Svg exposing (svg, text', text)
import Svg.Attributes exposing (width, height, x, y)


type alias Model = FactList.Model

viewBoxWidth : Float
viewBoxWidth = 1000

viewBoxHeight : Float
viewBoxHeight = 500

leftPadding : Float
leftPadding = 50

topPadding : Float
topPadding = 50

-- Specification for a chart

type alias Spec =
    Maybe
        { minX : Float
        , maxX : Float
        , maxY : Float
        , rects : List { left : Float, right : Float, height : Float }
        }


-- Produce a scaled and positioned spec for the chart

-- First, a mechanism to get an unscaled spec

rawSpec : FactList.Model -> Spec
rawSpec model =
    let
        layers = FactList.layers model
        intervals = Dist.intervals layers
        int2Rect int =
            { left = int.lower, right = int.upper, height = int.prob }
        rects = List.map int2Rect intervals
        maxHeight = List.map .height rects |> List.maximum
        toSpec (minX, maxX) maxY =
            { minX = minX, maxX = maxX, maxY = maxY, rects = rects }
    in
        Maybe.map2
            toSpec
            (Dist.range intervals)
            maxHeight

-- View

view : Model -> Html x
view model =
    svg
    [ width "100%"
    , height "600px"
    ]
    [ text'
      [ x "0"
      , y "100"
      ]
      [ model |> rawSpec |> toString |> text
      ]
    ]
