module Ram.Views.MainView exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (class)

import Ram.Views.Main.InputText exposing (inputTextArea)
import Ram.Views.Main.Instructions exposing (viewInstructions)
import Ram.Views.Main.Registers exposing (viewRegisters)

import Ram.Views.Main.OutputTape exposing (viewOutputTape)
import Ram.Views.Main.InputTape exposing (viewInputTape)

import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg)


mainContentView : Model -> Html Msg
mainContentView model =
    div [ class "flex flex-col mt-6 mb-3 gap-3 overflow-hidden" ]
        
        [ -- Top: Input Tape appears above everything.
        div []
            [
            viewInputTape model
            ]

          -- Middle: the three main sections (input text area, instructions, registers)
        , div [ class "flex gap-4 overflow-hidden" ]
            [ 
            inputTextArea model
            
            , div 
                [ class 
                    ( "flex flex-col w-1/3 p-3 shadow-lg rounded overflow-auto border-2 border-transparent "
                        ++ if ((model.instructionPointer >= List.length model.instructions) && model.simStarted) || model.halted then
                            " bg-green-50 border-green-400"
                        else
                            " bg-white"
                    )
                ]
                [ viewInstructions model.instructions model.instructionPointer ]

            , div [ class "flex flex-col w-1/3 bg-white p-3 shadow-lg rounded overflow-auto" ]
                ( viewRegisters model )
            ]
          
        -- Bottom: Output Tape appears below everything.
        , div []
            [
            viewOutputTape model
            ]
        ]
