module Fact exposing (Model, Msg, init, update, view)

import String
import Html exposing
    ( Html, Attribute
    , text, div, span, p
    , a
    , form, input, label, select, option
    )
import Html.Attributes exposing
    ( href
    , class, style, id
    , type', value, placeholder, for, selected
    )
import Html.Events exposing (onInput, on)
import Json.Decode

import Distribution exposing (Layer, Limit(AtMost, AtLeast))

-- Our model of a fact

type alias Model =
    { text : { probPerc : String, value : String }
    , data : Layer
    }

-- Things that can change: probability

type Msg
    = Prob String
    | Value String
    | ChangeLimit

-- Initial model

init : Model
init =
    { text = { probPerc = "10", value = "20" }
    , data = { prob = 0.10, limit = AtLeast, value = 20.0 }
    }

-- Updating the model

update : Msg -> Model -> Model
update msg model =
    case msg of
        Prob probStr ->
            updateProb probStr model
        Value valueStr ->
            updateValue valueStr model
        ChangeLimit ->
            toggleLimit model

updateProb : String -> Model -> Model
updateProb str model =
    let
        text = model.text
        data = model.data
        model' = { model | text = { text | probPerc = str } }
    in
        case (String.toFloat str) of
            Ok prob ->
                { model'
                | data = { data | prob = prob / 100 }
                }
            Err _ ->
                model'

updateValue : String -> Model -> Model
updateValue str model =
    let
        text = model.text
        data = model.data
        model' = { model | text = { text | value = str } }
    in
        case (String.toFloat str) of
            Ok value ->
                { model'
                | data = { data | value = value }
                }
            Err _ ->
                model'

toggleLimit : Model -> Model
toggleLimit model =
    let
        limit' =
            case (model.data.limit) of
                AtMost -> AtLeast
                AtLeast -> AtMost
        data = model.data
    in
        { model | data = { data | limit = limit' } }

-- Rendering a fact

view : Model -> Html Msg
view model =
    div []
    [ p [] [ formView model ]
    , p [] [ textView model ]
    ]

type alias TextBoxSpec =
    { id : String
    , label : String
    , width : String
    , value : String
    , mapping : (String -> Msg)
    }

textBox : TextBoxSpec -> Html Msg
textBox spec =
    div [ class "form-group" ]
    [ label
        [ for spec.id
        , class "sr-only"
        ]
        [ text spec.label ]
    , input
        [ type' "text"
        , id spec.id
        , class "form-control"
        , style [ ("width", spec.width), ("text-align", "right") ]
        , value spec.value
        , onInput spec.mapping
        ]
        []
    ]

probBox : Model -> Html Msg
probBox model =
    textBox
        { id = "probPerc"
        , label = "Percentage"
        , width = "5em"
        , value = model.text.probPerc
        , mapping = Prob
        }

valueBox : Model -> Html Msg
valueBox model =
    textBox
        { id = "probPerc"
        , label = "Percentage"
        , width = "7em"
        , value = model.text.value
        , mapping = Value
        }

onChange : msg -> Attribute msg
onChange message =
    on "change" (Json.Decode.succeed message)

limitControl : Model -> Html Msg
limitControl model =
    select
    [ class "form-control"
    , onChange ChangeLimit
    ]
    [ option
        [ selected (model.data.limit == AtMost) ]
        [ text "at most" ]
    , option
        [ selected (model.data.limit == AtLeast) ]
        [ text "at least" ]
    ]

formView : Model -> Html Msg
formView model =
        form
        [ class "form-inline" ]
        [ "There is a " |> text
        , probBox model
        , "% chance that it's " |> text
        , limitControl model
        , " " |> text
        , valueBox model
        ]

textView : Model -> Html Msg
textView model =
    model.data |> toString |> text

