module Ram.View exposing (..)

import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg(..))

import Ram.Components.Instructions exposing (viewInstructions)
import Ram.Components.Registers exposing (viewRegisters)
import Ram.Components.GuideModal exposing (viewGuideModal)
import Ram.Components.InputTape exposing (viewInputTape)
import Ram.Components.OutputTape exposing (viewOutputTape)
import Ram.Components.SettingsModal exposing (viewSettingsModal)

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
    { title = "HnatkoSim | RAM"
    , body =
        [ div [ class "flex flex-col h-screen p-1.5 bg-gray-200 gap-2" ]
            [ -- Header Section
              div [ class "flex flex-col lg:flex-row lg:gap-2" ]
                  [ controlButtons (model.instructionPointer >= List.length model.instructions) model.isRunning model.halted model.simStarted Start Pause Step Reset (model.speedIdx == 7 && ((model.executedInstructions >= model.totalMaxExecutedInstructions) || model.tooManyRuntimeMsgs ))
                  , speedSlider model.speedIdx model.speeds ChangeSpeed
                  , menuButtons ToggleSlotsModal GoBackToMenu ToggleGuideModal ToggleSettingsModal model.simStarted
                  ]

              -- Main Content Section
            , div [ class "flex flex-col h-full gap-2 overflow-hidden" ]
        
                  [ -- Input Tape
                  div []
                      [
                      viewInputTape model
                      ]

                    -- Main Section
                  , div [ class "flex flex-col overflow-y-auto md:flex-row gap-2 md:overflow-hidden" ]
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
                  viewSlotsModal (model.inputText == "") (Array.map (\slot -> ( slot.name, slot.inputText == "" )) model.slots) ToggleSlotsModal SaveSlot LoadSlot DeleteSlot UpdateSlotName NoOp TriggerUpload TriggerDownload
              else
                  text ""
            
                -- Guide Modal
            , if model.showGuideModal then
                viewGuideModal ToggleGuideModal LoadSlot NoOp
              else
                text ""
            
                -- Settings modal
            , if model.showSettingsModal then
                viewSettingsModal model
              else
                text ""

            ]
        ]
    }