module Shared.Components.ControlButtons exposing (controlButtons)

import Shared.Icons.Play exposing (heroiconPlay)
import Shared.Icons.Pause exposing (heroiconPause)
import Shared.Icons.Step exposing (heroiconStep)
import Shared.Icons.Reset exposing (heroiconReset)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)

controlButtons : Bool -> Bool -> Bool -> Bool -> msg -> msg -> msg -> msg -> Html msg
controlButtons atEndOfInstructions isRunning halted simStarted onStart onPause onStep onReset =
    div [ class "flex gap-4 w-1/3" ]
        [ -- Start/Pause Button
          if isRunning then
              button 
                  [ class "w-1/3 px-4 py-2 bg-blue-500 text-white flex items-center justify-center rounded"
                  , onClick onPause 
                  ]
                  [ heroiconPause, text "Pause" ]
          else
              button
                  ( [ class "w-1/3 px-4 py-2 bg-blue-500 text-white flex items-center justify-center rounded"
                    , onClick onStart
                    ]
                    ++ ( if halted || atEndOfInstructions then 
                            [ disabled True
                            , class "bg-gray-400 cursor-not-allowed"
                            ] 
                        else 
                            []
                    )
                  )
                  [ heroiconPlay, text "Start" ]

          -- Step Button
        , button
            ( [ class "w-1/3 px-4 py-2 bg-blue-500 text-white flex items-center justify-center rounded"
              , onClick onStep
              ]
              ++ ( if halted || isRunning || atEndOfInstructions then 
                      [ disabled True
                      , class "bg-gray-400 cursor-not-allowed"
                      ]
                  else
                      []
                )
            )
            [ heroiconStep, text "Step" ]

          -- Reset Button
        , if not simStarted then
              button 
                  [ class "w-1/3 px-4 py-2 bg-gray-400 text-white flex cursor-not-allowed items-center justify-center rounded"
                  , disabled True
                  ]
                  [ heroiconReset, text "Stop" ]
          else
              button 
                  [ class "w-1/3 px-4 py-2 bg-red-500 text-white flex items-center justify-center rounded"
                  , onClick onReset
                  ]
                  [ heroiconReset, text "Stop" ]
        ]
