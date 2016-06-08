module Chart exposing (view)

import FactList

import Html exposing (Html, p, text)


type alias Model = FactList.Model

view : Model -> Html x
view model =
    p []
    [ model |> toString |> text
    ]
