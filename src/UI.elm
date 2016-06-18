module UI exposing (Model, Msg, init, view, update)

import Distribution exposing (Layer)
import FactList
import Chart
import Errors

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
    let
        layers = FactList.layers model
    in
        div []
        [ FactList.view model
        , errorsOrChartView layers
        ]

errorsOrChartView : List Layer -> Html Msg
errorsOrChartView layers =
    let
        errs = Errors.errors layers
    in
        if (List.isEmpty errs) then
            Chart.layersToView layers
        else
            Errors.view errs

