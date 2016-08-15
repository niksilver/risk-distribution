module UI exposing (Model, Msg, init, view, update)

import Util exposing (singleton)
import Zone exposing (Zone)
import Constraints exposing (Segment, Constraint)
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
        constraintsModel = Constraints.model segments
    in
        div []
        [ FactList.view model
        , segmentsView segments
        , zonesView constraintsModel.zones
        , constraintsView constraintsModel.constraints
        ]

segmentsView : List Segment -> Html Msg
segmentsView segments =
    segments
        |> List.map (toString >> text >> singleton >> li [])
        |> ul []

zonesView : List Zone -> Html Msg
zonesView zones =
    zones
        |> List.map (toString >> text >> singleton >> li [])
        |> ul []

constraintsView : List Constraint -> Html Msg
constraintsView constraints =
    constraints
        |> List.map (Constraints.constraintToString >> text >> singleton >> li [])
        |> ul []
