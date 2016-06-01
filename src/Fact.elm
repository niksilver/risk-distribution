module Fact exposing (Model, Msg, init, update, view)

import String
import Html exposing (Html, text, div, span, p, input)
import Html.Attributes exposing (type', value)
import Html.Events exposing (onInput)

import Distribution exposing (Layer, Limit(AtMost, AtLeast))

-- Our model of a fact

type alias Model =
    { text : { probPerc : String }
    , data : Layer
    }

-- Things that can change: probability

type Msg = Prob String

-- Initial model

init : Model
init =
    { text = { probPerc = "10" }
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

limitString : Model -> String
limitString model =
    case model.data.limit of
        AtLeast -> "at least"
        AtMost -> "at most"

formView : Model -> Html Msg
formView model =
        span []
        [ "There is a " |> text
        , probBox model
        , "% chance that it's "
            ++ (limitString model)
            ++ " "
            ++ (model.data.value |> toString)
            |> text
        ]

textView : Model -> Html Msg
textView model =
    model.data |> toString |> text

