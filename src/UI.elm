module UI exposing (Model, Msg, init, view, update)

import FactList
import Chart

import Html exposing (Html, div, p, text)


type alias Model = FactList.Model

type alias Msg = FactList.Msg


-- Initialisation

init : Model
init =
    FactList.init


-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    FactList.update msg model


-- View

view : Model -> Html Msg
view model =
    div []
    [ FactList.view model
    , Chart.view model
    ]

