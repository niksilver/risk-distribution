module Chart exposing (Model, Msg, init, view)

import Html exposing (Html, text)

import Distribution exposing (Interval(Closed))

type alias Model = List Interval

type Msg = Ping

init : Model
init =
    [ Closed { lower = 0, upper = 10, prob = 0.10 }
    , Closed { lower = 10, upper = 50, prob = 0.60 }
    , Closed { lower = 50, upper = 100, prob = 0.30 }
    ]

view : Model -> Html Msg
view model =
    model |> toString |> text
