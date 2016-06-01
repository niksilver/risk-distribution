module Fact exposing (Model, Msg, init, view)

import Html exposing (Html, text, div)

import Distribution exposing (Interval(Closed))

type alias Model = Interval

type Msg = Ping

init : Model
init =
    Closed { lower = 0, upper = 10, prob = 0.10 }

view : Model -> Html Msg
view model =
    div []
    [ textView model
    ]

textView : Model -> Html Msg
textView model =
    case model of
        Closed cls ->
            "There is a " ++ (cls.prob |> toString)
            ++ " probability that it's between "
            ++ (cls.lower |> toString) ++ " and "
            ++ (cls.upper |> toString)
            |> text

