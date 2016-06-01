module Fact exposing (Model, Msg, init, update, view)

import String
import Html exposing (Html, text, div, span, p, input)
import Html.Attributes exposing (type', value)
import Html.Events exposing (onInput)

import Distribution exposing (Interval)

-- Our model of a fact

type alias Model =
    { text : { probPerc : String }
    , data : Interval
    }

-- Things that can change: probability

type Msg = Prob String

-- Initial model

init : Model
init =
    { text = { probPerc = "10" }
    , data = { lower = 0, upper = 10, prob = 0.10 }
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
    in
        case (String.toFloat str) of
            Ok prob ->
                { model
                | text = { text | probPerc = str }
                , data = { data | prob = prob / 100 }
                }
            Err _ ->
                { model
                | text = { text | probPerc = str }
                }

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

formView : Model -> Html Msg
formView model =
        span []
        [ "There is a " |> text
        , probBox model
        , "% chance that it's between "
            ++ (model.data.lower |> toString) ++ " and "
            ++ (model.data.upper |> toString)
            |> text
        ]

textView : Model -> Html Msg
textView model =
    model.data |> toString |> text

