module Fact exposing (Model, Msg, init, view)

import Html exposing (Html, text, div, span, input)
import Html.Attributes exposing (type', value)

import Distribution exposing (Interval(Closed))

type alias Model = Interval

type Msg = Ping

init : Model
init =
    Closed { lower = 0, upper = 10, prob = 0.10 }

view : Model -> Html Msg
view model =
    div []
    [ formView model
    ]

probBox : Float -> Html Msg
probBox prob =
    input
    [ type' "input", value (toString prob) ]
    []

formView : Model -> Html Msg
formView model =
    case model of
        Closed cls ->
            span []
            [ "There is a " |> text
            , probBox cls.prob
            , " probability that it's between "
                ++ (cls.lower |> toString) ++ " and "
                ++ (cls.upper |> toString)
                |> text
            ]

