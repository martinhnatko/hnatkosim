module Am.View exposing (..)

import Am.Types.Model exposing (Model)
import Am.Types.Messages exposing (Msg(..))

import Am.Components.Instructions exposing (viewInstructions)
import Am.Components.Registers exposing (viewRegisters)

import Shared.Components.SlotsModal exposing (viewSlotsModal)
import Shared.Components.Console exposing (viewConsole)
import Shared.Components.MenuButtons exposing (menuButtons)
import Shared.Components.ControlButtons exposing (controlButtons)
import Shared.Components.SpeedSlider exposing (speedSlider)
import Shared.Components.InputTextArea exposing (inputTextArea)

import Html exposing (div, text)
import Html.Attributes exposing (class)

import Browser


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "AM Simulator"
    , body =
        [ div [ class "flex flex-col h-screen p-2 bg-gray-200" ]
            [ -- Header Section
              div [ class "flex gap-4" ]
                    [ controlButtons (model.instructionPointer >= List.length model.instructions) model.isRunning False model.simStarted Start Pause Step Reset
                    , speedSlider model.speedIdx model.speeds ChangeSpeed
                    , menuButtons ToggleSlotsModal
                    ]


              -- Main Content Section
            , div [ class "flex mt-5 mb-3 gap-4 overflow-hidden" ]
                  
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
                  viewSlotsModal model.inputText model.slots ToggleSlotsModal SaveSlot LoadSlot DeleteSlot
              else
                  text ""
            ]
        ]
    }