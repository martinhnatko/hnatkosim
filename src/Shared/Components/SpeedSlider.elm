module Shared.Components.SpeedSlider exposing (..)

import Shared.Icons.Rocket exposing (heroiconRocket)

import Html exposing (Html, li, span, text, ul, div, input)
import Html.Attributes exposing (type_, step, value, class)
import Html.Events exposing (onInput)

import Array exposing (Array)

speedSlider : Int -> Array Int -> (Int -> msg) -> Html msg
speedSlider currentValue speeds onChangeSpeed =
    div [ class "flex flex-col lg:w-1/3 lg:mb-0.5 mb-5 lg:h-12 px-5 font-mono order-2 lg:order-2" ]
        [ -- Title with Rocket Icon
          div [ class "flex items-center mt-1 lg:mt-0 gap-1 text-gray-700 text-xs" ]
            [ heroiconRocket
            , text "Time between instructions (seconds)"
            ]

        , -- Slider Input
          input
            [ type_ "range"
            , class "w-full"
            , Html.Attributes.min "1"
            , Html.Attributes.max "7"
            , step "1"
            , value (String.fromInt currentValue)
            , onInput (String.toInt >> Maybe.withDefault 1 >> onChangeSpeed)
            ]
            []

        , -- Labels for the Slider
          ul [ class "flex justify-between w-full px-[11px] text-sm" ]
            (List.indexedMap sliderLabel (Array.toList speeds))
        ]
        

-- Transform your slider speeds from ms to s in the labels
sliderLabel : Int -> Int -> Html msg
sliderLabel index ms =
    let
        toOneDecimal f = (toFloat (round (f * 10))) / 10

        inSeconds = toOneDecimal ((toFloat ms) / 1000)

        label = 
          (
            if index == 6 then
              "Instant"
            else
              String.fromFloat inSeconds
          )

    in
    li [ class "flex justify-center relative" ]
        [ span [ class "absolute" ]
            [ text label ]
        ]


