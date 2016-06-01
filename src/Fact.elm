module Fact exposing (Model, Msg, init, view)

import Html exposing (Html, text)

import Distribution exposing (Interval(Closed))

type alias Model = Interval

type Msg = Ping

init : Model
init =
    Closed { lower = 0, upper = 10, prob = 0.10 }

view : Model -> Html Msg
view model =
    model |> toString |> text
