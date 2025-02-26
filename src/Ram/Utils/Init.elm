module Ram.Utils.Init exposing (..)
import Dict
import List exposing (range)
import Array

-- INIT

init =
    let
        initialModel =
            { inputText = ""
            , registers = Dict.fromList (List.map (\n -> (n, 0)) (range 0 100))
            , highlighted_registers = Dict.empty
            , highlighted_input_tape = Dict.empty
            , highlighted_output_tape = Dict.empty
            , instructions = []
            , isRunning = False
            , simStarted = False
            , instructionPointer = 0
            , speeds = Array.fromList [ 4000 , 2000, 1000, 500, 300, 100, 0 ]
            , speedIdx = 4
            , consoleMessages = []
            , slots = Array.fromList
                [ { name = "", inputText = "", inputTape = Array.empty }
                , { name = "Slot 1", inputText = "", inputTape = Array.empty }
                , { name = "Slot 2", inputText = "", inputTape = Array.empty }
                , { name = "Slot 3", inputText = "", inputTape = Array.empty }
                , { name = "Slot 4", inputText = "", inputTape = Array.empty }
                , { name = "Slot 5", inputText = "", inputTape = Array.empty }
                , { name = "Slot 6", inputText = "", inputTape = Array.empty }
                , { name = "Slot 7", inputText = "", inputTape = Array.empty }
                , { name = "Slot 8", inputText = "", inputTape = Array.empty }
                , { name = "Slot 9", inputText = "", inputTape = Array.empty }
                , { name = "Slot 10", inputText = "", inputTape = Array.empty }
                , { name = "Slot 11", inputText = "", inputTape = Array.empty }
                , { name = "Slot 12", inputText = "", inputTape = Array.empty }
                , { name = "Slot 13", inputText = "", inputTape = Array.empty }
                , { name = "Slot 14", inputText = "", inputTape = Array.empty }
                , { name = "Slot 15", inputText = "", inputTape = Array.empty }
                , { name = "Slot 16", inputText = "", inputTape = Array.empty }
                , { name = "Slot 17", inputText = "", inputTape = Array.empty }
                , { name = "Slot 18", inputText = "", inputTape = Array.empty }
                , { name = "Slot 19", inputText = "", inputTape = Array.empty }
                , { name = "Slot 20", inputText = "", inputTape = Array.empty }
                ]
            , halted = False
            , inputTape = Array.empty
            , inputTapePointer = 0
            , outputTape = Array.empty
            , showSlotsModal = False
            , showGuideModal = False
            , simulationStartTime = Nothing
            , executedInstructions = 0
            }
    in
    initialModel
