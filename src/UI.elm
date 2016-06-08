module UI exposing (Model, Msg, init, view, update)

import FactList as FL

import Html exposing (Html, text)


type alias Model = FL.Model

type alias Msg = FL.Msg

init : Model
init =
    FL.init

view : Model -> Html Msg
view model =
    FL.view model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    FL.update msg model

