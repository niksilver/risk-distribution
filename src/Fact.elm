module Fact exposing
    ( Model, Msg (ProbPerc, Value, ChangeLimit, ConfirmText)
    , init, layer, update, view
    )

-- A statement describing the probability of some value range

import String
import Html exposing
    ( Html, Attribute
    , text, div, span
    , a
    , form, input, label, select, option, button
    )
import Html.Attributes exposing
    ( href
    , class, style, id
    , type', value, placeholder, for, selected
    )
import Html.Events exposing (onInput, onClick, on)
import Json.Decode exposing (Decoder)

import Distribution exposing (Layer, Limit(AtMost, AtLeast))

-- Our model of a fact

type alias Model =
    { text : { probPerc : String, limit : Limit, value : String }
    , data : Layer
    }

-- Things that can change

type Msg
    = ProbPerc String
    | Value String
    | ChangeLimit Limit
    | ConfirmText

-- Initial model

init : Layer -> Model
init y =
    { text =
        { probPerc = y.prob * 100 |> toString
        , limit = y.limit
        , value = y.value |> toString
        }
    , data = y
    }

-- Extract the layer

layer : Model -> Layer
layer =
    .data

-- Updating the model

update : Msg -> Model -> Model
update msg model =
    case msg of
        ProbPerc probStr ->
            updateProbPerc probStr model
        Value valueStr ->
            updateValue valueStr model
        ChangeLimit limit ->
            updateLimit limit model
        ConfirmText ->
            updateText model

updateProbPerc : String -> Model -> Model
updateProbPerc str model =
    let
        text = model.text
    in
        { model | text = { text | probPerc = str } }

updateValue : String -> Model -> Model
updateValue str model =
    let
        text = model.text
    in
        { model | text = { text | value = str } }

updateLimit : Limit -> Model -> Model
updateLimit limit model =
    let
        text = model.text
    in
        { model | text = { text | limit = limit } }

updateText : Model -> Model
updateText model =
    let
        probPerc' = String.toFloat model.text.probPerc
        value' = String.toFloat model.text.value
        limit' = model.text.limit
    in
        case (probPerc', value') of
            (Ok prob, Ok val) ->
                { model
                | data =
                    { prob = prob / 100
                    , limit = limit'
                    , value = val
                    }
                }
            _ ->
                model

-- Rendering a fact

view : Model -> Html Msg
view model =
        span []
        [ "There is a " |> text
        , probBox model
        , "% chance that it's " |> text
        , limitControl model
        , " " |> text
        , valueBox model
        , okayView
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
        , mapping = ProbPerc
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

onChange : Attribute Msg
onChange =
    let
        strDecoder = Json.Decode.at ["target", "value"] Json.Decode.string
        toLimit str =
            if str == "at most" then
                ChangeLimit AtMost
            else
                ChangeLimit AtLeast
    in
        on "change" (Json.Decode.map toLimit strDecoder)

limitControl : Model -> Html Msg
limitControl model =
    select
    [ class "form-control"
    , onChange
    ]
    [ option
        [ selected (model.data.limit == AtMost) ]
        [ text "at most" ]
    , option
        [ selected (model.data.limit == AtLeast) ]
        [ text "at least" ]
    ]

okayView : Html Msg
okayView =
    button
    [ class "btn btn-default"
    , type' "button"
    , style [ ("margin-left", "3em") ]
    , onClick ConfirmText
    ]
    [ text "Okay" ]

