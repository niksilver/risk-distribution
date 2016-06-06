module FactList exposing (Model, Msg, init, update, view)

import Html exposing (Html, div)
import Html.App as App

import Fact

type alias Model =
    { next : Int
    , iFacts : List IndexedFact
    }

type alias IndexedFact = { id : Int, fact : Fact.Model }

type Msg
    = ToFact Int Fact.Msg

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

updateFact : Int -> Fact.Msg -> Model -> Model
updateFact factId factMsg model =
    let
        updateOne {id, fact} =
            IndexedFact
                id
                (if (id /= factId) then fact else (Fact.update factMsg fact |> fst))
    in
        { model | iFacts = List.map updateOne model.iFacts }

view : Model -> Html Msg
view model =
    div []
        (List.map factView model.iFacts)

factView : IndexedFact -> Html Msg
factView { id, fact } =
    App.map (ToFact id) (Fact.view fact)

