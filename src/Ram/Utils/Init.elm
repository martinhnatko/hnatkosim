module Ram.Utils.Init exposing (..)
import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg)
import Dict
import List exposing (range)
import Array
import Shared.Ports exposing (getItem)

-- INIT

init : { inputText : String, registers : Dict.Dict Int number, highlighted : Dict.Dict k v, instructions : List a, isRunning : Bool, simStarted : Bool, instructionPointer : number, speeds : Array.Array number, speedIdx : number, consoleMessages : List b, slots : Array.Array String, showSlotsModal : Bool, labels : Dict.Dict c d, halted : Bool, inputTape : Array.Array e, inputTapePointer : number, outputTape : Array.Array f }
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
            , showSlotsModal = False
            , labels = Dict.empty
            , halted = False
            , inputTape = Array.empty
            , inputTapePointer = 0
            , outputTape = Array.empty
            }
    in
    initialModel
