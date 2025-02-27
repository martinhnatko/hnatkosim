module Shared.Components.MenuButtons exposing (menuButtons)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)

import Shared.Icons.Save exposing (heroiconSave)
import Shared.Icons.Menu exposing (heroiconMenu)
import Shared.Icons.Guide exposing (heroiconGuide)

menuButtons : msg -> msg -> msg -> Bool -> Html msg
menuButtons onToggleSlotsModal onGoBackToMenu onToggleGuideModal simStarted =
    div [ class "flex gap-4 w-1/3" ]
        [ -- Save/Load Slots Button
          if not simStarted then
              button
                  [ class "w-1/3 px-1 py-2 border border-blue-500 text-blue-500 bg-white flex items-center justify-center rounded gap-1 hover:bg-blue-50 transition-colors duration-200 focus:outline-none"
                  , onClick onToggleSlotsModal
                  ]
                  [ heroiconSave, text "Save/Load" ]
          else
              button
                  [ class "w-1/3 px-1 py-2 border border-gray-400 text-gray-400 bg-gray-100 flex items-center justify-center rounded gap-1 cursor-not-allowed"
                  , disabled True
                  ]
                  [ heroiconSave, text "Save/Load" ]
          
          -- Guide Button
        , if not simStarted then 
              button
                  [ class "w-1/3 px-1 py-2 border border-blue-500 text-blue-500 bg-white flex items-center justify-center rounded gap-1 hover:bg-blue-50 transition-colors duration-200 focus:outline-none"
                  , onClick onToggleGuideModal
                  ]
                  [ heroiconGuide, text "Guide" ]
          else
              button
                  [ class "w-1/3 px-1 py-2 border border-gray-400 text-gray-400 bg-gray-100 flex items-center justify-center rounded gap-1 cursor-not-allowed"
                  , disabled True
                  ]
                  [ heroiconGuide, text "Guide" ]
          
          -- Menu Button
        , if not simStarted then
              button
                  [ class "w-1/3 px-1 py-2 border border-blue-500 text-blue-500 bg-white flex items-center justify-center rounded gap-1 hover:bg-blue-50 transition-colors duration-200 focus:outline-none"
                  , onClick onGoBackToMenu
                  ]
                  [ heroiconMenu, text "Menu" ]
          else
              button
                  [ class "w-1/3 px-1 py-2 border border-gray-400 text-gray-400 bg-gray-100 flex items-center justify-center rounded gap-1 cursor-not-allowed"
                  , disabled True
                  ]
                  [ heroiconMenu, text "Menu" ]
        ]
