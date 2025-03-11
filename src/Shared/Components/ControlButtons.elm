module Shared.Components.ControlButtons exposing (controlButtons)

import Shared.Icons.Play exposing (heroiconPlay)
import Shared.Icons.Pause exposing (heroiconPause)
import Shared.Icons.Step exposing (heroiconStep)
import Shared.Icons.Reset exposing (heroiconReset)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)

controlButtons : Bool -> Bool -> Bool -> Bool -> msg -> msg -> msg -> msg -> Bool -> Html msg
controlButtons atEndOfInstructions isRunning halted simStarted onStart onPause onStep onReset startDisabled =
    div [ class "flex gap-3 lg:w-1/3 font-mono text-lg order-3 lg:order-1 min-h-[3rem]" ]
        [ -- Start/Pause Button
            if isRunning then
                button 
                    [ class "w-1/3 px-2 py-1 bg-blue-500 text-white flex items-center justify-center rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                    , onClick onPause 
                    ]
                    [ heroiconPause, text "Pause" ]
            else
                (
                if halted || atEndOfInstructions || startDisabled then 
                    button 
                        [ class "w-1/3 px-2 py-1 bg-gray-400 text-white flex items-center justify-center rounded cursor-not-allowed"
                        , disabled True
                        ]
                        [ heroiconPlay, text "Start" ]
                else
                    button
                        ( [ class "w-1/3 px-2 py-1 bg-blue-500 text-white flex items-center justify-center rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                          , onClick onStart
                          ]
                        )
                        [ heroiconPlay, text "Start" ]
                )

        ,(
        if halted || isRunning || atEndOfInstructions then 
            button 
                [ class "w-1/3 px-2 py-1 bg-gray-400 text-white flex items-center justify-center rounded cursor-not-allowed"
                , disabled True
                ]
                [ heroiconStep, text "Step" ]
        else
            button
                [ class "w-1/3 px-2 py-1 bg-blue-500 text-white flex items-center justify-center rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                , onClick onStep
                ]
                [ heroiconStep, text "Step" ]
        )

          -- Reset Button
        , if not simStarted then
              button 
                  [ class "w-1/3 px-2 py-1 bg-gray-400 text-white flex items-center justify-center rounded cursor-not-allowed"
                  , disabled True
                  ]
                  [ heroiconReset, text "Stop" ]
          else
              button 
                  [ class "w-1/3 px-2 py-1 bg-red-500 text-white flex items-center justify-center rounded hover:bg-red-600 transition-colors duration-200 focus:outline-none"
                  , onClick onReset
                  ]
                  [ heroiconReset, text "Stop" ]
        ]
