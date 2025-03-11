module Shared.Components.MenuButtons exposing (menuButtons)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)

import Shared.Icons.Save exposing (heroiconSave)
import Shared.Icons.Menu exposing (heroiconMenu)
import Shared.Icons.Guide exposing (heroiconGuide)
import Shared.Icons.Settings exposing (heroiconSettings)

menuButtons : msg -> msg -> msg -> msg -> Bool -> Html msg
menuButtons onToggleSlotsModal onGoBackToMenu onToggleGuideModal onToggleSettingsModal simStarted =
    div [ class "flex gap-3 lg:w-1/3 font-mono text-lg order-1 lg:order-3 min-h-[3rem]" ]
        [ -- Save/Load Slots Button
          if not simStarted then
              button
                  [ class "w-1/3 px-1 py-0 border border-blue-500 text-blue-500 bg-white flex items-center justify-center rounded gap-1 hover:bg-blue-50 transition-colors duration-200 focus:outline-none"
                  , onClick onToggleSlotsModal
                  ]
                  [ heroiconSave, text "Save/Load" ]
          else
              button
                  [ class "w-1/3 px-1 py-0 border border-gray-400 text-gray-400 bg-gray-100 flex items-center justify-center rounded gap-1 cursor-not-allowed"
                  , disabled True
                  ]
                  [ heroiconSave, text "Save/Load" ]
          
          -- Guide Button
        , if not simStarted then 
              button
                  [ class "w-1/3 px-2 py-1 border border-blue-500 text-blue-500 bg-white flex items-center justify-center rounded gap-1 hover:bg-blue-50 transition-colors duration-200 focus:outline-none"
                  , onClick onToggleGuideModal
                  ]
                  [ heroiconGuide, text "Guide" ]
          else
              button
                  [ class "w-1/3 px-2 py-1 border border-gray-400 text-gray-400 bg-gray-100 flex items-center justify-center rounded gap-1 cursor-not-allowed"
                  , disabled True
                  ]
                  [ heroiconGuide, text "Guide" ]
        
        , div [ class "w-1/3 flex gap-3" ] 
            [
                -- Settings Button
                if not simStarted then
                    button
                        [ class "w-1/2 px-2 py-1 border border-blue-500 text-blue-500 bg-white flex items-center justify-center rounded gap-1 hover:bg-blue-50 transition-colors duration-200 focus:outline-none"
                        , onClick onToggleSettingsModal
                        ]
                        [ heroiconSettings ]
                else
                    button
                        [ class "w-1/2 px-2 py-1 border border-gray-400 text-gray-400 bg-gray-100 flex items-center justify-center rounded gap-1 cursor-not-allowed"
                        , disabled True
                        ]
                        [ heroiconSettings ]


                -- Menu Button
                , button
                    [ class "w-1/2 px-2 py-1 border border-blue-500 text-blue-500 bg-white flex items-center justify-center rounded gap-1 hover:bg-blue-50 transition-colors duration-200 focus:outline-none"
                    , onClick onGoBackToMenu
                    ]
                    [ heroiconMenu ]
            ]
          
        ]
