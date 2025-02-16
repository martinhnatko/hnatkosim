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
            , highlighted = Dict.empty
            , instructions = []
            , isRunning = False
            , simStarted = False
            , instructionPointer = 0
            , speeds = Array.fromList [ 4000 , 2000, 1000, 500, 300, 100, 0 ]
            , speedIdx = 4
            , consoleMessages = []
            , slots = Array.repeat 21 ""
            , slots_input_tapes = Array.repeat 21 Array.empty
            , showSlotsModal = False
            , labels = Dict.empty
            , halted = False
            , inputTape = Array.empty
            , inputTapePointer = 0
            , outputTape = Array.empty
            }
    in
    initialModel
