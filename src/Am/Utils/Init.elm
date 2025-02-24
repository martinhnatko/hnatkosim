module Am.Utils.Init exposing (..)
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
            , slots = Array.fromList
                [ { name = "", inputText = "" }
                , { name = "Slot 1", inputText = "" }
                , { name = "Slot 2", inputText = "" }
                , { name = "Slot 3", inputText = "" }
                , { name = "Slot 4", inputText = "" }
                , { name = "Slot 5", inputText = "" }
                , { name = "Slot 6", inputText = "" }
                , { name = "Slot 7", inputText = "" }
                , { name = "Slot 8", inputText = "" }
                , { name = "Slot 9", inputText = "" }
                , { name = "Slot 10", inputText = "" }
                , { name = "Slot 11", inputText = "" }
                , { name = "Slot 12", inputText = "" }
                , { name = "Slot 13", inputText = "" }
                , { name = "Slot 14", inputText = "" }
                , { name = "Slot 15", inputText = "" }
                , { name = "Slot 16", inputText = "" }
                , { name = "Slot 17", inputText = "" }
                , { name = "Slot 18", inputText = "" }
                , { name = "Slot 19", inputText = "" }
                , { name = "Slot 20", inputText = "" }
                ]
            , showSlotsModal = False
            }
        
    in
    initialModel