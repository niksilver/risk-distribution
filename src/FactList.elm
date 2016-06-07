module FactList exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, p, button, text)
import Html.Attributes exposing (class, type')
import Html.Events exposing (onClick)
import Html.App as App

import Fact

type alias Model =
    { next : Int
    , iFacts : List IndexedFact
    }

type alias IndexedFact = { id : Int, fact : Fact.Model }

type Msg
    = ToFact Int Fact.Msg
    | Add
    | Remove Int

init : Model
init =
    { next = 1
    , iFacts = [ { id = 0, fact = Fact.init } ]
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToFact id factMsg ->
            (updateFact id factMsg model, Cmd.none)
        Add ->
            (addFact model, Cmd.none)
        Remove id ->
            (removeFact id model, Cmd.none)

updateFact : Int -> Fact.Msg -> Model -> Model
updateFact factId factMsg model =
    let
        updateOne {id, fact} =
            IndexedFact
                id
                (if (id == factId) then (Fact.update factMsg fact) else fact)
    in
        { model | iFacts = List.map updateOne model.iFacts }

addFact : Model -> Model
addFact model =
    { next = model.next + 1
    , iFacts = List.append model.iFacts [ { id = model.next, fact = Fact.init } ]
    }

removeFact : Int -> Model -> Model
removeFact removeId model =
    let
        keep { id, fact } =
            id /= removeId
    in
        { model
        | iFacts = List.filter keep model.iFacts 
        }

view : Model -> Html Msg
view model =
    div []
        (List.append
            (List.map (\f -> p [] [ removableFactView f ]) model.iFacts)
            [ p [] [ addView ]
            , p [] [ textView model ]
            ]
        )

removableFactView : IndexedFact -> Html Msg
removableFactView iFact =
    div
    [ class "form-inline" ]
    [ factView iFact
    , removeView iFact
    ]

factView : IndexedFact -> Html Msg
factView { id, fact } =
    App.map (ToFact id) (Fact.view fact)

removeView : IndexedFact -> Html Msg
removeView { id, fact } =
    button
    [ class "btn btn-default"
    , type' "button"
    , onClick (Remove id)
    ]
    [ text "Remove" ]


addView : Html Msg
addView =
    button
    [ class "btn btn-default"
    , type' "button"
    , onClick Add
    ]
    [ text "Add" ]

textView : Model -> Html Msg
textView model =
    model |> toString |> text

