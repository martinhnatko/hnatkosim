module Am.View exposing (..)

import Am.Types.Model exposing (Model)
import Am.Types.Messages exposing (Msg(..))

import Am.Components.Instructions exposing (viewInstructions)
import Am.Components.Registers exposing (viewRegisters)
import Am.Components.GuideModal exposing (viewGuideModal)
import Am.Components.SettingsModal exposing (viewSettingsModal)

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
    { title = "HnatkoSim | AM"
    , body =
        [ div [ class "flex flex-col h-screen p-2 bg-gray-200" ]
            [ -- Header Section
              div [ class "flex gap-3" ]
                    [ controlButtons (model.instructionPointer >= List.length model.instructions) model.isRunning False model.simStarted Start Pause Step Reset (model.speedIdx == 7 && (model.executedInstructions >= model.totalMaxExecutedInstructions))
                    , speedSlider model.speedIdx model.speeds ChangeSpeed
                    , menuButtons ToggleSlotsModal GoBackToMenu ToggleGuideModal ToggleSettingsModal model.simStarted
                    ]


              -- Main Content Section
            , div [ class "flex my-3 h-full gap-3 overflow-hidden" ]
                  
                  -- Input text area
                  [ inputTextArea model.simStarted model.inputText UpdateCode DeleteInput

                  -- Instructions
                  , viewInstructions model.instructions model.instructionPointer model.simStarted

                  -- Registers
                  , viewRegisters model.registers model.highlighted
                  ]


              -- Console Section
            , viewConsole model.consoleMessages

              -- Slots Modal
            , if model.showSlotsModal then
                  viewSlotsModal (model.inputText == "") (Array.map (\slot -> ( slot.name, slot.inputText == "" )) model.slots) ToggleSlotsModal SaveSlot LoadSlot DeleteSlot UpdateSlotName NoOp
              else
                  text ""
            ]

              -- Guide modal
            , if model.showGuideModal then
                viewGuideModal ToggleGuideModal LoadSlot NoOp
              else
                text ""

              -- Settings modal
            , if model.showSettingsModal then
                viewSettingsModal model.totalNumberOfRegisters model.totalMaxExecutedInstructions ToggleSettingsModal ChangeNumOfRegisters ChangeMaxExecutedInstructions TypedRegsNum TypedMaxExecutedInstructions model.typedTotalNumberOfRegisters model.typedTotalMaxExecutedInstructions NoOp
              else
                text ""
        ]
    }