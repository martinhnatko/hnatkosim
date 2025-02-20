module Ram.Utils.Init exposing (..)
import Dict
import List exposing (range)
import Array

-- INIT


init : { inputText : String, registers : Dict.Dict Int number, highlighted_registers : Dict.Dict k v, highlighted_input_tape : Dict.Dict a b, highlighted_output_tape : Dict.Dict c d, instructions : List e, isRunning : Bool, simStarted : Bool, instructionPointer : number, speeds : Array.Array number, speedIdx : number, consoleMessages : List f, slots : Array.Array String, slots_input_tapes : Array.Array (Array.Array g), showSlotsModal : Bool, halted : Bool, inputTape : Array.Array h, inputTapePointer : number, outputTape : Array.Array i }
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
            , slots = Array.repeat 21 ""
            , slots_input_tapes = Array.repeat 21 Array.empty
            , showSlotsModal = False
            , halted = False
            , inputTape = Array.empty
            , inputTapePointer = 0
            , outputTape = Array.empty
            }
    in
    initialModel
