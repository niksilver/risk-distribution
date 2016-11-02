module Fact exposing
    ( Model
    , Limit (AtLeast, AtMost, Between)
    , Msg (ProbPerc, Lower, Upper, ChangeLimit, ConfirmText)
    , init, segment, update, view
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

import Zone exposing (inf, Zone)
import Segment exposing (Segment)


-- Our model of a fact

type Limit
    = AtLeast
    | AtMost
    | Between

type alias Model =
    { text : { probPerc : String, limit : Limit, lower : String, upper : String }
    , data : Segment
    }

-- Things that can change

type Msg
    = ProbPerc String
    | Lower String
    | Upper String
    | ChangeLimit Limit
    | ConfirmText


-- Constants

atLeastString : String
atLeastString = "more than"

atMostString : String
atMostString = "less than"


-- Initial model

init : Segment -> Model
init y =
    let
        toString2 num =
            if (num == inf || num == -inf) then "" else toString num
        limit' =
            if (y.zone.from == -inf) then
                AtMost
            else if (y.zone.to == inf) then
                AtLeast
            else Between
    in
        { text =
            { probPerc = y.pc |> toString
            , limit = limit'
            , lower = y.zone.from |> toString2
            , upper = y.zone.to |> toString2
            }
        , data = y
        }

-- Extract the segment

segment : Model -> Segment
segment =
    .data

-- Updating the model

update : Msg -> Model -> Model
update msg model =
    case msg of
        ProbPerc probStr ->
            updateProbPerc probStr model
        Lower lowerStr ->
            updateLower lowerStr model
        Upper upperStr ->
            updateUpper upperStr model
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

updateLower : String -> Model -> Model
updateLower str model =
    let
        text = model.text
    in
        { model | text = { text | lower = str } }

updateUpper : String -> Model -> Model
updateUpper str model =
    let
        text = model.text
    in
        { model | text = { text | upper = str } }

updateLimit : Limit -> Model -> Model
updateLimit newLimit model =
    let
        text = model.text
        oldLimit = text.limit
        (lower', upper') =
            case (oldLimit, newLimit) of
                (AtLeast, AtMost) -> (text.upper, text.lower)
                (AtMost, AtLeast) -> (text.upper, text.lower)
                (Between, AtLeast) -> (text.lower, "")
                (Between, AtMost) -> ("", text.upper)
                _ -> (text.lower, text.upper)
    in
        { model
        | text =
            { text
            | limit = newLimit
            , lower = lower'
            , upper = upper'
            }
        }

updateText : Model -> Model
updateText model =
    let
        probPerc' = String.toInt model.text.probPerc
        lower' =
            if (model.text.limit == AtMost) then
                Ok -inf
            else
                String.toFloat model.text.lower
        upper' =
            if (model.text.limit == AtLeast) then
                Ok inf
            else
                String.toFloat model.text.upper
        limit' = model.text.limit
        comparison' = Result.map2 compare lower' upper'
    in
        case (probPerc', lower', upper', comparison') of
            (Ok prob, Ok lower, Ok upper, Ok LT) ->
                { model
                | data = Segment prob (Zone lower upper)
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
        , valueBoxes model
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

valueBoxes : Model -> Html Msg
valueBoxes model =
    let
        span' =
            span [ style [ ("display", "inline-block"), ("width", "19em") ]]
    in
        case model.text.limit of
            AtLeast ->
                span' [ lowerBox model.text.lower ]
            AtMost ->
                span' [ upperBox model.text.upper ]
            Between ->
                span'
                    [ lowerBox model.text.lower
                    , " and " |> text
                    , upperBox model.text.upper
                    ]

lowerBox : String -> Html Msg
lowerBox lower =
    textBox
        { id = "lower"
        , label = "Value"
        , width = "7em"
        , value = lower
        , mapping = Lower
        }

upperBox : String -> Html Msg
upperBox upper =
    textBox
        { id = "upper"
        , label = "Value"
        , width = "7em"
        , value = upper
        , mapping = Upper
        }

onChange : Attribute Msg
onChange =
    let
        strDecoder = Json.Decode.at ["target", "value"] Json.Decode.string
        toLimit str =
            if str == atMostString then
                ChangeLimit AtMost
            else if str == atLeastString then
                ChangeLimit AtLeast
            else ChangeLimit Between
    in
        on "change" (Json.Decode.map toLimit strDecoder)

limitControl : Model -> Html Msg
limitControl model =
    let
        limit =
            if (model.data.zone.to == inf) then
                AtLeast
            else if (model.data.zone.from == -inf) then
                AtMost
            else
                Between
    in
        select
        [ class "form-control"
        , onChange
        ]
        [ option
            [ selected (limit == AtMost) ]
            [ text atMostString ]
        , option
            [ selected (limit == AtLeast) ]
            [ text atLeastString ]
        , option
            [ selected (limit == Between) ]
            [ text "between" ]
        ]

okayView : Html Msg
okayView =
    button
    [ class "btn btn-default"
    , type' "button"
    , style [ ("margin-left", "1em") ]
    , onClick ConfirmText
    ]
    [ text "Okay" ]
