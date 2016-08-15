module UI exposing (Model, Msg, init, view, update)

import Util exposing (singleton)
import Constraints exposing (Segment)
import FactList

import Html exposing (Html, div, p, ul, li, text)


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
    let
        segments = FactList.segments model
    in
        div []
        [ FactList.view model
        , segmentsView segments
        , constraintsView segments
        ]

segmentsView : List Segment -> Html Msg
segmentsView segments =
    segments
        |> List.map (toString >> text >> singleton >> li [])
        |> ul []

constraintsView : List Segment -> Html Msg
constraintsView segments =
    segments
        |> Constraints.model
        |> .constraints
        |> List.map (Constraints.constraintToString >> text >> singleton >> li [])
        |> ul []
