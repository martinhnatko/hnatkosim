module Shared.Components.MenuButtons exposing (menuButtons)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Shared.Icons.Save exposing (heroiconSave)

menuButtons : msg -> Html msg
menuButtons onToggleSlotsModal =
    div [ class "flex gap-4 w-1/3" ]
        [ -- Save/Load Slots Button
          button
              [ class "border border-blue-500 text-blue-500 bg-white w-1/3 px-1 py-2 flex items-center justify-center rounded"
              , onClick onToggleSlotsModal
              ]
              [ heroiconSave, text "Save/Load" ]
        ]


