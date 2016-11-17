module Chart exposing (view)

import Axis exposing (Scale)
import Spec exposing (Spec, ViewDims, Transformer)
import Curve  -- Only needed when we want to render an actual curve
import Block exposing (Rect)
import Value exposing (Value (Exactly, Maximum, Contradiction))

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SvgA


maxTicks : Int
maxTicks = 6

-- Dimensions for the viewBox

viewDim : ViewDims
viewDim =
    { left = 50
    , right = 50
    , top = 20
    , bottom = 80
    , width = 800
    , height = 350
    }

-- Produce a scaled and positioned spec for the chart

-- View

view : Spec -> Html x
view spec =
    let
        -- Rescale the chart spec to include an x-axis with nice max and min

        scale = Axis.scale spec.minX spec.maxX maxTicks
        scaledSpec =
            { spec
            | minX = scale.min
            , maxX = scale.max
            }
        transformer = Spec.transformer viewDim scaledSpec

        viewBoxDim =
            "0 0 "
            ++ (viewDim.left + viewDim.right + viewDim.width |> toString) ++ " "
            ++ (viewDim.top + viewDim.bottom + viewDim.height |> toString)
    in
        Svg.svg
        [ SvgA.width "100%"
        , SvgA.height "400px"
        , SvgA.viewBox viewBoxDim
        ]
        [ viewBlocks transformer scaledSpec
        , Axis.viewXAxis transformer scale
        ]

-- Render just the distribution area as chart bars,
-- given functions to transform and scale a given spec

viewBlocks : Transformer -> Spec -> Svg x
viewBlocks transformer spec =
    let
        draw (value, bar) =
            case value of
                Exactly _ _ ->
                    drawExactly transformer bar
                Maximum pc _ ->
                    drawMaximum transformer bar pc
                _ ->
                    Svg.svg [] []
        valBar block =
            List.map (\bar -> (block.overlay.value, bar)) block.bars
        valBars =
            spec.blocks
                |> List.map valBar
                |> List.concat
    in
        Svg.g
        []
        (List.map draw valBars)

drawExactly : Transformer -> Rect -> Svg x
drawExactly transformer rect =
    Svg.rect
    [ SvgA.x (rect.left |> transformer.trX |> toString)
    , SvgA.y (rect.height |> transformer.trY |> toString)
    , SvgA.width (rect.right - rect.left |> transformer.scX |> toString)
    , SvgA.height (rect.height |> transformer.scY |> toString)
    , SvgA.fill "rgb(82, 92, 190)"
    ]
    []

drawMaximum : Transformer -> Rect -> Int -> Svg x
drawMaximum transformer rect pc =
    let
        x1 = rect.left |> transformer.trX
        x2 = rect.right |> transformer.trX
        -- To calculate the y value, the 0th y value is at the full height,
        -- the 1th y value is is bit over half of that, the 2th is a bit over
        -- half of that, etc.
        y idx =
            rect.height / (1.5 ^ toFloat idx) |> transformer.trY
        drawLine idx =
            Svg.line
            [ SvgA.x1 (x1 |> toString)
            , SvgA.y1 (y idx |> toString)
            , SvgA.x2 (x2 |> toString)
            , SvgA.y2 (y idx |> toString)
            , SvgA.stroke "rgb(128, 128, 0)"
            ]
            []
        indexCount = max 3 (pc |> toFloat |> sqrt |> round)
        indices = [0..indexCount]
    in
        Svg.g
        []
        (List.map drawLine indices)
