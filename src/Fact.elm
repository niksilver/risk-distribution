module Fact exposing (Model, Msg, init, update, view)

import String
import Html exposing (Html, text, div, span, p, input)
import Html.Attributes exposing (type', value)
import Html.Events exposing (onInput)

import Distribution exposing (Interval(Closed))

-- Our model of a fact

type alias Model =
    { lower : Float, upper : Float, prob : Float }

-- Things that can change: probability

type Msg = Prob String

-- Initial model

init : Model
init =
    { lower = 0, upper = 10, prob = 0.10 }

-- Updating the model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Prob probStr ->
            ( { model | prob = parseProb probStr model.prob }
            , Cmd.none
            )

parseProb : String -> Float -> Float
parseProb str default =
    Result.withDefault default (String.toFloat str)

-- Rendering a fact

view : Model -> Html Msg
view model =
    div []
    [ p [] [ formView model ]
    , p [] [ textView model ]
    ]

probBox : Float -> Html Msg
probBox prob =
    input
    [ type' "input"
    , value (toString prob)
    , onInput Prob
    ]
    []

formView : Model -> Html Msg
formView model =
        span []
        [ "There is a " |> text
        , probBox model.prob
        , " probability that it's between "
            ++ (model.lower |> toString) ++ " and "
            ++ (model.upper |> toString)
            |> text
        ]

textView : Model -> Html Msg
textView model =
    model |> toString |> text
