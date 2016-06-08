import UI

import Html.App exposing (program)


type alias Model = UI.Model

type alias Msg = UI.Msg

main : Program Never
main =
    program
        { init = (UI.init, Cmd.none)
        , view = UI.view
        , update = UI.update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

