module UI exposing (Model, Msg, init, view, update)

import FactList as FL

import Html exposing (Html, div, p, text)


type alias Model = FL.Model

type alias Msg = FL.Msg


-- Initialisation

init : Model
init =
    FL.init


-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    FL.update msg model


-- View

view : Model -> Html Msg
view model =
    div []
    [ FL.view model
    , textView model
    ]

textView : Model -> Html Msg
textView model =
    p []
    [ model |> toString |> text
    ]

