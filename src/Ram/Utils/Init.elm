module Ram.Utils.Init exposing (..)
import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg)
import Dict
import List exposing (range)
import Array
import Shared.Ports exposing (getItem)

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
            , speeds = Array.fromList [ 4000 , 2000, 1000, 500, 250, 100, 0 ]
            , speedIdx = 4
            , consoleMessages = []
            , slots = Array.repeat 21 ""
            , showSlotsModal = False
            , labels = Dict.empty
            , halted = False
            , inputTape = Array.repeat 100 Nothing
            , inputTapePointer = 0
            , outputTape = Array.repeat 100 Nothing
            , outputTapePointer = 0
            , maybeFocusedCell = Nothing
            , pendingFocus = Nothing
            }
    in
    initialModel
