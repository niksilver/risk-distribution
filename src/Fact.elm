module Fact exposing (Model, Msg, init, update, view)

import String
import Html exposing
    ( Html
    , text, div, span, p
    , a
    , input
    )
import Html.Attributes exposing (type', href, value)
import Html.Events exposing (onClick, onInput)

import Distribution exposing (Layer, Limit(AtMost, AtLeast))

-- Our model of a fact

type alias Model =
    { text : { probPerc : String, value : String }
    , data : Layer
    }

-- Things that can change: probability

type Msg
    = Prob String
    | Value String
    | ChangeLimit

-- Initial model

init : Model
init =
    { text = { probPerc = "10", value = "20" }
    , data = { prob = 0.10, limit = AtLeast, value = 20.0 }
    }

-- Updating the model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Prob probStr ->
            ( updateProb probStr model
            , Cmd.none
            )
        Value valueStr ->
            ( updateValue valueStr model
            , Cmd.none
            )
        ChangeLimit ->
            ( toggleLimit model
            , Cmd.none
            )

updateProb : String -> Model -> Model
updateProb str model =
    let
        text = model.text
        data = model.data
        model' = { model | text = { text | probPerc = str } }
    in
        case (String.toFloat str) of
            Ok prob ->
                { model'
                | data = { data | prob = prob / 100 }
                }
            Err _ ->
                model'

updateValue : String -> Model -> Model
updateValue str model =
    let
        text = model.text
        data = model.data
        model' = { model | text = { text | value = str } }
    in
        case (String.toFloat str) of
            Ok value ->
                { model'
                | data = { data | value = value }
                }
            Err _ ->
                model'

toggleLimit : Model -> Model
toggleLimit model =
    let
        limit' =
            case (model.data.limit) of
                AtMost -> AtLeast
                AtLeast -> AtMost
        data = model.data
    in
        { model | data = { data | limit = limit' } }

-- Rendering a fact

view : Model -> Html Msg
view model =
    div []
    [ p [] [ formView model ]
    , p [] [ textView model ]
    ]

probBox : Model -> Html Msg
probBox model =
    input
    [ type' "input"
    , value model.text.probPerc
    , onInput Prob
    ]
    []

valueBox : Model -> Html Msg
valueBox model =
    input
    [ type' "input"
    , value model.text.value
    , onInput Value
    ]
    []

limitControl : Model -> Html Msg
limitControl model =
    let
        content =
            case model.data.limit of
                AtLeast -> "at least"
                AtMost -> "at most"
    in
        a
        [ href "#"
        , onClick ChangeLimit
        ]
        [ content |> text ]

formView : Model -> Html Msg
formView model =
        span []
        [ "There is a " |> text
        , probBox model
        , "% chance that it's " |> text
        , limitControl model
        , " " |> text
        , valueBox model
        ]

textView : Model -> Html Msg
textView model =
    model.data |> toString |> text

