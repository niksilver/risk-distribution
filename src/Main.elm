import FactList

import Html.App exposing (program)

type alias Model = FactList.Model

type alias Msg = FactList.Msg

main : Program Never
main =
    program
        { init = (FactList.init, Cmd.none)
        , view = FactList.view
        , update = FactList.update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

