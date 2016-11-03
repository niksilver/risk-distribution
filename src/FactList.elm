module FactList exposing (Model, Msg, init, segments, update, view)

import Html exposing (Html, p, ol, li, button, text)
import Html.Attributes exposing (style, class, type')
import Html.Events exposing (onClick)
import Html.App as App

import Fact exposing (Limit (AtLeast, AtMost, Between))
import Zone exposing (inf, Zone)
import Segment exposing (Segment)


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
    { next = 2
    , iFacts =
        [ { id = 0
          , fact = Fact.init (Segment 100 (Zone 0 10))
          }
        ]
    }


-- Extract the raw segments from the model

segments : Model -> List Segment
segments model =
    model.iFacts
        |> List.map .fact
        |> List.map Fact.segment


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
        lastIFact = model.iFacts |> List.reverse |> List.head
        -- The new iFact is the same as the last (if there is one)
        newIFact =
            case lastIFact of
                Nothing ->
                    { id = model.next
                    , fact = Fact.init (Segment 100 (Zone 0 inf))
                    }
                Just iFact ->
                    { id = model.next
                    , fact = iFact.fact
                    }
    in
        { next = model.next + 1
        , iFacts = List.append model.iFacts [ newIFact ]
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
    ol []
        (List.append
            (List.map removableFactView model.iFacts)
            [ p [] [ addView ]
            ]
        )

removableFactView : IndexedFact -> Html Msg
removableFactView iFact =
    li
    [ class "form-inline"
    , style [ ("padding-top", "0.5em"), ("padding-bottom", "0.5em") ]
    ]
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
    , style [ ("margin-left", "1em") ]
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
