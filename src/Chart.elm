module Chart exposing (view)

import FactList

import Html exposing (Html)
import Svg exposing (svg, text', text)
import Svg.Attributes exposing (width, height, viewBox, x, y)


type alias Model = FactList.Model


view : Model -> Html x
view model =
    svg
    [ width "800px"
    , height "600px"
    , viewBox "0 0 800 600"
    ]
    [ text'
      [ x "100"
      , y "100"
      ]
      [ model |> toString |> text
      ]
    ]
