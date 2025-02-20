module Am.Utils.Init exposing (..)
import Dict
import List exposing (range)
import Array

-- INIT

init : { inputText : String, registers : Dict.Dict Int number, highlighted : Dict.Dict k v, instructions : List a, isRunning : Bool, simStarted : Bool, instructionPointer : number, speeds : Array.Array number, speedIdx : number, consoleMessages : List b, slots : Array.Array String, showSlotsModal : Bool }
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
            }
        
    in
    initialModel