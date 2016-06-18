module FactList exposing (Model, Msg, init, layers, update, view)

import Html exposing (Html, div, p, button, text)
import Html.Attributes exposing (style, class, type')
import Html.Events exposing (onClick)
import Html.App as App

import Fact
import Distribution as Dist exposing (Limit(AtLeast, AtMost))


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
    , iFacts =
        [ { id = 0
          , fact = Fact.init { prob = 1.0, limit = AtLeast, value = 0.0 }
          }
        , { id = 1
          , fact = Fact.init { prob = 1.0, limit = AtMost, value = 10.0 }
          }
        ]
    }


-- Extract the raw layers from the model

layers : Model -> List Dist.Layer
layers model =
    model.iFacts
        |> List.map .fact
        |> List.map Fact.layer


-- Updates

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
    let
        newFact =
            { id = model.next
            , fact = Fact.init { prob = 0, limit = AtLeast, value = 1.0 }
            }
    in
        { next = model.next + 1
        , iFacts = List.append model.iFacts [ newFact ]
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


-- Views

view : Model -> Html Msg
view model =
    div []
        (List.append
            (List.map removableFactView model.iFacts)
            [ p [] [ addView ]
            ]
        )

removableFactView : IndexedFact -> Html Msg
removableFactView iFact =
    p
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
    , style [ ("margin-left", "3em") ]
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

