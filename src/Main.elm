import Html exposing (Html)
import Html.App exposing (program)

type alias Model = String

type Msg = Ping

main : Program Never
main =
    program
        { init = ("Hello, world!", Cmd.none)
        , view = view
        , update = (\msg model -> (model, Cmd.none))
        , subscriptions = subscriptions
        }

view : Model -> Html Msg
view model =
    Html.text model

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

