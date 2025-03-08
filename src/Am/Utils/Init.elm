module Am.Utils.Init exposing (..)
import Dict
import List exposing (range)
import Array

-- INIT
init =
    let
        initialModel =
            { inputText = ""
            , registers = Dict.fromList (List.map (\n -> (n, (0,  False))) (range 0 100))
            , highlighted = Dict.empty
            , instructions = []
            , isRunning = False
            , simStarted = False
            , instructionPointer = 0
            , speeds = Array.fromList [ 4000 , 2000, 1000, 500, 200, 0, 0 ]
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

                , { name = "", inputText = "# a + b\n# Result: R3\n# Destructive approach\n\n# Load 'a' into R1\na1a1a1a1\n\n# Load 'b' into R2\na2a2\n\n# Transfer 'a' from R1 to R3\n(s1 a3)1\n\n# Transfer 'b' from R2 to R3\n(s2 a3)2" }
                , { name = "", inputText = "# a * b\n# Result: R3\n# Destructive approach\n\n# Load 'a' into R1 and 'b' into R2\na1a1a1\na2a2a2a2\n\n# Loop of copying 'a' from R1 do R3 and decrementing 'b' till it's 0\n(\n\t# Copy 'a' from R1 to R3 (via helper register R10).\n\t(s1 a3 a10)1\n\t(s10 a1)10\n\n\t# Decrement 'b'\n\ts2\t\n)2" }
                , { name = "", inputText = "# n!\n# Result: R2\n# Destructive approach\n\n# Load 'n' into R1 (n = 4)\na1a1a1a1\n\n# Initialize R2 to 1 (accumulator for the factorial result)\na2\n\n# Outer loop: while R1 is nonzero, multiply R2 by R1 and then decrement R1\n(\n\n    # Inner multiplication loop: repeats for each unit in R2\n    (\n\n        # Transfer value from R1 to R3 and R4.\n        (s1 a3 a4)1\n        \n        # Decrement R2 to account for one iteration of multiplication.\n        s2\n        \n        # Restore R1 by transferring the value from R4 back to R1.\n        (s4 a1)4\n\n    )2\n    \n    # Transfer the accumulated value from R3 into R2.\n    # This effectively multiplies the old R2 by the original value of R1.\n    (s3 a2)3\n    \n    # Decrement R1 by 1 to prepare for the next multiplication step.\n    s1\n\n)1\n" }
                ]
            , showSlotsModal = False
            , showGuideModal = False
            , showSettingsModal = False
            , simulationStartTime = Nothing
            , executedInstructions = 0
            , totalNumberOfRegisters = 100
            , totalMaxExecutedInstructions = 1000000
            , typedTotalNumberOfRegisters = ""
            , typedTotalMaxExecutedInstructions = ""
            }
        
    in
    initialModel