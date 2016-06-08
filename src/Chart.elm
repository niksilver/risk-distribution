module Chart exposing (view)

import FactList

import Html exposing (Html)
import Svg exposing (svg, text', text)
import Svg.Attributes exposing (width, height, x, y)


type alias Model = FactList.Model


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
      [ model |> toString |> text
      ]
    ]
