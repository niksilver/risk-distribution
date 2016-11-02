module UI exposing (Model, Msg, init, view, update)

import Util exposing (singleton)
import Zone exposing (Zone)
import Segment exposing (Segment)
import Derivation exposing (Derivation)
import DerivationScheme exposing (Scheme)
import ZoneDict
import Spec
import FactList
import Chart

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
        dScheme = DerivationScheme.scheme segments
    in
        div []
        [ FactList.view model
        , chartView segments
        --, segmentsView dScheme.segments
        --, zonesView dScheme.zones
        --, derivationsView dScheme.derivations
        , valuesView dScheme.zones dScheme.derivations
        ]

segmentsView : List Segment -> Html Msg
segmentsView segments =
    bulletListView toString segments

zonesView : List Zone -> Html Msg
zonesView zones =
    bulletListView toString zones

derivationsView : List Derivation -> Html Msg
derivationsView derivations =
    bulletListView Derivation.derivationToString derivations

valuesView : List Zone -> List Derivation -> Html Msg
valuesView zones derivations =
    ZoneDict.fill zones derivations
        |> ZoneDict.toList
        |> bulletListView toString

bulletListView : (a -> String) -> List a -> Html Msg
bulletListView trans xs =
    xs
        |> List.map (trans >> text >> singleton >> li [])
        |> ul []

chartView : List Segment -> Html Msg
chartView segments =
    case (Spec.fromSegments segments) of
        Just spec ->
            Chart.view spec
        Nothing ->
            text "No chart"
