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
        cModel = Constraints.model segments
    in
        div []
        [ FactList.view model
        , segmentsView cModel.segments
        , zonesView cModel.zones
        , constraintsView cModel.constraints
        ]

segmentsView : List Segment -> Html Msg
segmentsView segments =
    bulletListView toString segments

zonesView : List Zone -> Html Msg
zonesView zones =
    bulletListView toString zones

constraintsView : List Constraint -> Html Msg
constraintsView constraints =
    bulletListView Constraints.constraintToString constraints

bulletListView : (a -> String) -> List a -> Html Msg
bulletListView trans xs =
    xs
        |> List.map (trans >> text >> singleton >> li [])
        |> ul []
