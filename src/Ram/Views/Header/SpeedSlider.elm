module Ram.Views.Header.SpeedSlider exposing (..)
import Html exposing (Html, li, span, text, ul, div, input)
import Html.Attributes exposing (type_, step, value)
import Html.Events exposing (onInput)
import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg(..))
import Shared.Icons.Rocket exposing (heroiconRocket)
import Array

-- Transform your slider speeds from ms to s in the labels
sliderLabel : Int -> Html msg
sliderLabel ms =
    let
        toOneDecimal f =
            (toFloat (round (f * 10))) / 10

        inSeconds =
            toOneDecimal ((toFloat ms) / 1000)

        label =
            if ms == 0 then
                "0"
            else
                String.fromFloat inSeconds
    in
    li [ Html.Attributes.class "flex justify-center relative" ]
        [ span [ Html.Attributes.class "absolute" ]
            [ text label ]
        ]


speedSlider : Int -> Model -> Html Msg
speedSlider currentValue model =
    div [ Html.Attributes.class "flex flex-col p-1 w-1/3" ]
        [ -- Title with Rocket Icon
          div [ Html.Attributes.class "flex items-center gap-2 text-gray-700" ]
            [ heroiconRocket
            , text "Time between instructions (seconds)"
            ]

        , -- Slider Input
          input
            [ type_ "range"
            , Html.Attributes.class "w-full"
            , Html.Attributes.min "1"
            , Html.Attributes.max "7"
            , step "1"
            , value (String.fromInt currentValue)
            , onInput (String.toInt >> Maybe.withDefault 1 >> ChangeSpeed)
            ]
            []

        , -- Labels for the Slider
          ul [ Html.Attributes.class "flex justify-between w-full px-[10px]" ]
            (List.map sliderLabel (Array.toList model.speeds))
        ]
        
