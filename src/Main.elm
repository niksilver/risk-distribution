import Chart

import Html.App exposing (program)

type alias Model = Chart.Model

type alias Msg = Chart.Msg

main : Program Never
main =
    program
        { init = (Chart.init, Cmd.none)
        , view = Chart.view
        , update = (\msg model -> (model, Cmd.none))
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

