module Chart exposing
    ( Spec
    , rawSpec
    , layersToView, view
    )

import FactList
import Distribution as Dist

import Html exposing (Html)
import Svg exposing (svg, text', text)
import Svg.Attributes exposing (width, height, x, y)


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
    { minX : Float
    , maxX : Float
    , maxY : Float
    , rects : List { left : Float, right : Float, height : Float }
    }


-- Produce a scaled and positioned spec for the chart

-- First, a mechanism to get an unscaled spec.
-- The area of each rectangle will be its probability

rawSpec : List Dist.Layer -> Maybe Spec
rawSpec layers =
    let
        intervals = layers |> Dist.intervals |> Dist.sort
        int2Rect int =
            { left = int.lower
            , right = int.upper
            , height = int.prob / (int.upper - int.lower)
            }
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

layersToView : List Dist.Layer -> Html x
layersToView layers =
    case (rawSpec layers) of
        Just spec -> view spec
        Nothing -> text ""
 
view : Spec -> Html x
view spec =
    svg
    [ width "100%"
    , height "600px"
    ]
    [ text'
      [ x "0"
      , y "100"
      ]
      [ spec |> toString |> text
      ]
    ]
