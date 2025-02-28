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
            , speeds = Array.fromList [ 4000 , 2000, 1000, 500, 200, 0, 0 ]
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

                , { name = "", inputText = "# Average of two numbers\n\n# Read numbers from input tape into R1 and R2\nread 1\nread 2\n\n# Add numbers into accumulator\nadd 1\nadd 2\n\n# Divide numbers by 2\ndiv =2\n\n# Write the result to the output tape\nwrite 0\n\nhalt", inputTape = Array.fromList [48,152] }
                , { name = "", inputText = "# a^n\n\n# Read 'a' and 'n' into R1 and R2\nread 1\nread 2\n\n# Load 1 into helper register R3\nload =1\nstore 3\n\n# Multiply R3 by R1 and decrement R2\n# Loops until R2 is greater than 0\ncyklus:\n\tload 3\n\tmul 1\n\tstore 3\n\n\tload 2\n\tsub =1\n\tstore 2\n\n\tJGTZ cyklus\n\n# Write result from R3 in the output tape\nwrite 3\nhalt", inputTape = Array.fromList [ 5, 4 ] }
                , { name = "", inputText = "# n!\n\n# Read number from input tape into R1\nread 1\n\n# Load number from R1 into accumulator\nload 1\n\n# If it's zero, jump to label zero, because 0! is 1 \njzero zero\n\n# Load constant 1 into accumulator and store it in R2\nload =1\nstore 2\n\n# Load number from R2, multiply it by input number and decrement input number\n# Loops until the input value is zero\nloop:\n\tload 2\n\tmul 1\n\tstore 2\n\n\tload 1\n\tsub =1\n\tstore 1\n\t\n\tJGTZ loop\n\n# Reslt is in R2, write it in the output tape and halt\nwrite 2\nhalt\n\n# 0! is 1, so write it in the output tape and halt\nzero:\n\tload =1\n\twrite 0\n\thalt", inputTape = Array.fromList [ 5 ] }
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
