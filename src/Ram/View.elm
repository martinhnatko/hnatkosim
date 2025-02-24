module Ram.View exposing (..)

import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg(..))

import Ram.Components.Instructions exposing (viewInstructions)
import Ram.Components.Registers exposing (viewRegisters)

import Ram.Components.InputTape exposing (viewInputTape)
import Ram.Components.OutputTape exposing (viewOutputTape)

import Shared.Components.SlotsModal exposing (viewSlotsModal)
import Shared.Components.Console exposing (viewConsole)
import Shared.Components.MenuButtons exposing (menuButtons)
import Shared.Components.ControlButtons exposing (controlButtons)
import Shared.Components.SpeedSlider exposing (speedSlider)
import Shared.Components.InputTextArea exposing (inputTextArea)

import Html exposing (div, text)
import Html.Attributes exposing (class)

import Browser
import Array


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "RAM Simulator"
    , body =
        [ div [ class "flex flex-col h-screen p-2 bg-gray-200" ]
            [ -- Header Section
              div [ class "flex gap-4" ]
                  [ controlButtons (model.instructionPointer >= List.length model.instructions) model.isRunning model.halted model.simStarted Start Pause Step Reset
                  , speedSlider model.speedIdx model.speeds ChangeSpeed
                  , menuButtons ToggleSlotsModal GoBackToMenu model.simStarted
                  ]

              -- Main Content Section
            , div [ class "flex flex-col my-3 gap-3 overflow-hidden" ]
        
                  [ -- Input Tape
                  div []
                      [
                      viewInputTape model
                      ]

                    -- Main Section
                  , div [ class "flex gap-3 overflow-hidden" ]
                      [ 
                      -- Input text area
                      inputTextArea model.simStarted model.inputText UpdateCode DeleteInput
                      
                      -- Instructions
                      , viewInstructions model.instructions model.instructionPointer model.simStarted model.halted

                      -- Registers
                      , viewRegisters model
                      ]
                    
                  -- Output Tape
                  , div []
                      [
                      viewOutputTape model
                      ]
                  ]

              -- Console section
            , viewConsole model.consoleMessages 

              -- Slots Modal
            , if model.showSlotsModal then
                  viewSlotsModal (model.inputText == "") (Array.map (\slot -> ( slot.name, slot.inputText == "" )) model.slots) ToggleSlotsModal SaveSlot LoadSlot DeleteSlot UpdateSlotName
              else
                  text ""
            ]
        ]
    }