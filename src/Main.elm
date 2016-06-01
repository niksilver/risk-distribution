import Fact

import Html.App exposing (program)

type alias Model = Fact.Model

type alias Msg = Fact.Msg

main : Program Never
main =
    program
        { init = (Fact.init, Cmd.none)
        , view = Fact.view
        , update = (\msg model -> (model, Cmd.none))
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

