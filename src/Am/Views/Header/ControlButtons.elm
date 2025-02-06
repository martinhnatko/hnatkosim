module Am.Views.Header.ControlButtons exposing (controlButtons)
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Shared.Icons.Play exposing (heroiconPlay)
import Shared.Icons.Pause exposing (heroiconPause)
import Shared.Icons.Step exposing (heroiconStep)
import Shared.Icons.Reset exposing (heroiconReset)
import Am.Types.Messages exposing (Msg(..))
import Am.Types.Model exposing (Model)


controlButtons : Model -> Html Msg
controlButtons model =
    let
        atEndOfInstructions : Bool
        atEndOfInstructions =
            model.instructionPointer >= List.length model.instructions

        isRunning = model.isRunning
        isDisabledStart = atEndOfInstructions
        isDisabledStep = atEndOfInstructions || model.isRunning
    in
    div [ class "flex gap-4 w-1/3" ]
        [ -- Start/Pause Button
          if isRunning then
              button 
                  [ class "w-1/3 px-4 py-2 bg-blue-500 text-white flex items-center justify-center rounded"
                  , onClick Pause 
                  ]
                  [ heroiconPause, text "Pause" ]
          else
              button
                  ( [ class "w-1/3 px-4 py-2 bg-blue-500 text-white flex items-center justify-center rounded"
                    , onClick Start
                    ]
                    ++ ( if isDisabledStart then 
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
              , onClick Step
              ]
              ++ ( if isDisabledStep then 
                      [ disabled True
                      , class "bg-gray-400 cursor-not-allowed"
                      ]
                  else
                      []
                )
            )
            [ heroiconStep, text "Step" ]

          -- Reset Button
        , if not model.simStarted then
              button 
                  [ class "w-1/3 px-4 py-2 bg-gray-400 text-white flex cursor-not-allowed items-center justify-center rounded"
                  , disabled True
                  ]
                  [ heroiconReset, text "Stop" ]
          else
              button 
                  [ class "w-1/3 px-4 py-2 bg-red-500 text-white flex items-center justify-center rounded"
                  , onClick Reset
                  ]
                  [ heroiconReset, text "Stop" ]
        ]
