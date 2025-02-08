module Ram.Utils.ExecuteInstruction exposing (..)

import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Instructions exposing (Instruction(..))
import Ram.Types.Operand exposing (Operand(..))
import Ram.Utils.HelperFunctions exposing (..)

import Dict
import Task
import Process
import Array
import Platform.Cmd as Cmd

executeInstruction : Model -> Int -> (Model, Cmd Msg)
executeInstruction model highlightDuration =
    let
        currentInstruction =
            List.head (List.drop model.instructionPointer model.instructions)

        nextInstructionPointer =
            model.instructionPointer + 1

    in
    case currentInstruction of
        Nothing ->
            -- No more instructions to execute, stop the simulator
            ( { model | halted = True, isRunning = False }, Cmd.none )

        Just instr ->
            case instr of
                
                -- LOAD: Update register 0 (accumulator) with the operand value.
                Load operand ->
                    case operand of
                        Constant n ->
                            ( model
                                |> updateRegister 0 n
                                |> incrementIP
                            , Cmd.none 
                            )

                        Direct regIndex ->
                            let
                                value = getRegisterValue regIndex model
                            in
                            (
                            model
                                |> updateRegister 0 value
                                |> incrementIP
                            , Cmd.none 
                            )

                        Indirect regIndex ->
                            let
                                pointer = getRegisterValue regIndex model
                                value = getRegisterValue pointer model
                            in
                            (
                            model
                                |> updateRegister 0 value
                                |> incrementIP
                            , Cmd.none 
                            )

                -- STORE: Store the accumulator (register 0) into the given register.
                Store operand ->
                    case operand of
                        Constant _ ->
                            -- A STORE with a constant operand doesn't make sense; do nothing.
                            (
                            model |> incrementIP
                            , Cmd.none 
                            )

                        Direct regIndex ->
                            let
                                accVal = getRegisterValue 0 model
                            in
                            (
                            model
                                |> updateRegister regIndex accVal
                                |> incrementIP
                            , Cmd.none 
                            )

                        Indirect regIndex ->
                            let
                                pointer = getRegisterValue regIndex model
                                accVal = getRegisterValue 0 model
                            in
                            (
                            model
                                |> updateRegister pointer accVal
                                |> incrementIP
                            , Cmd.none 
                            )
                -- ADD: Add the operand value to the accumulator.
                Add operand ->
                    let
                        accVal = getRegisterValue 0 model
                        value =
                            case operand of
                                Constant n ->
                                    n

                                Direct regIndex ->
                                    getRegisterValue regIndex model

                                Indirect regIndex ->
                                    let
                                        pointer = getRegisterValue regIndex model
                                    in
                                    getRegisterValue pointer model
                    in
                    (
                    model
                        |> updateRegister 0 (accVal + value)
                        |> incrementIP
                    , Cmd.none 
                    )

                -- SUB: Subtract the operand value from the accumulator.
                Sub operand ->
                    let
                        accVal = getRegisterValue 0 model
                        value =
                            case operand of
                                Constant n ->
                                    n

                                Direct regIndex ->
                                    getRegisterValue regIndex model

                                Indirect regIndex ->
                                    let
                                        pointer = getRegisterValue regIndex model
                                    in
                                    getRegisterValue pointer model
                    in
                    (
                    model
                        |> updateRegister 0 (accVal - value)
                        |> incrementIP
                    , Cmd.none 
                    )

                -- MUL: Multiply the accumulator by the operand.
                Mul operand ->
                    let
                        accVal = getRegisterValue 0 model
                        value =
                            case operand of
                                Constant n ->
                                    n

                                Direct regIndex ->
                                    getRegisterValue regIndex model

                                Indirect regIndex ->
                                    let
                                        pointer = getRegisterValue regIndex model
                                    in
                                    getRegisterValue pointer model
                    in
                    (
                    model
                        |> updateRegister 0 (accVal * value)
                        |> incrementIP
                    , Cmd.none 
                    )    

                -- DIV: Divide the accumulator by the operand, if nonzero.
                Div operand ->
                    let
                        accVal = getRegisterValue 0 model
                        value =
                            case operand of
                                Constant n ->
                                    n

                                Direct regIndex ->
                                    getRegisterValue regIndex model

                                Indirect regIndex ->
                                    let
                                        pointer = getRegisterValue regIndex model
                                    in
                                    getRegisterValue pointer model
                    in
                    if value /= 0 then
                        (
                        model
                            |> updateRegister 0 (accVal // value)
                            |> incrementIP
                        , Cmd.none 
                        )    
                    else
                        -- Division by zero; simply skip the operation.
                        (
                        model |> incrementIP
                        , Cmd.none 
                        )

                -- JUMP: Set the instruction pointer to the target label.
                Jump idx _ ->
                    ( { model | instructionPointer = idx }
                    , Cmd.none 
                    )
                -- JZERO: Jump if the accumulator is zero.
                Jzero idx _ ->
                    if getRegisterValue 0 model == 0 then
                        ( { model | instructionPointer = idx }
                        , Cmd.none 
                        )
                    else
                        ( model |> incrementIP
                        , Cmd.none 
                        )

                -- JGTZ: Jump if the accumulator is greater than zero.
                Jgtz idx _ ->
                    if getRegisterValue 0 model > 0 then
                        ( { model | instructionPointer = idx }
                        , Cmd.none 
                        )
                    else
                        ( model |> incrementIP
                        , Cmd.none 
                        )

                -- HALT: End the program.
                Halt ->
                    ( { model | halted = True, isRunning = False }
                    , Cmd.none 
                    )

                -- LABEL: Do nothing (labels are stored in model.labels).
                Label _ ->
                    ( model |> incrementIP
                    , Cmd.none 
                    )

                UnknownInstruction ->
                    ( model |> incrementIP
                    , Cmd.none 
                    )

                Read operand ->
                    case operand of
                        Constant _ ->
                            let
                                newIP =
                                    model.instructionPointer + 1
                            in
                            ( { model
                                | instructionPointer = newIP
                            }
                            , Cmd.none
                            )

                        Direct regIndex ->
                            case Array.get model.inputTapePointer model.inputTape of
                                Just (Just val) ->
                                    let
                                        newIP = model.instructionPointer + 1

                                        newTapePointer = model.inputTapePointer + 1
                                        
                                        updatedModel =
                                            model
                                                |> updateRegister regIndex val
                                            
                                    in
                                    ( { updatedModel |
                                        instructionPointer = newIP
                                        , inputTapePointer = newTapePointer
                                    }
                                    , Cmd.none
                                    )

                                _ ->
                                    -- ERRORRRRRRRRRRRRRRRRRRR
                                    ( model |> incrementIP , Cmd.none )

                        Indirect regIndex ->
                            let
                                pointer = getRegisterValue regIndex model
                            in
                            case Array.get model.inputTapePointer model.inputTape of
                                Just (Just val) ->
                                    let
                                        newIP = model.instructionPointer + 1

                                        newTapePointer = model.inputTapePointer + 1
                                        
                                        updatedModel =
                                            model
                                                |> updateRegister pointer val
                                            
                                    in
                                    ( { updatedModel |
                                        instructionPointer = newIP
                                        , inputTapePointer = newTapePointer
                                    }
                                    , Cmd.none
                                    )

                                _ ->
                                    -- ERRORRRRRRRRRRRRRRRRRRR
                                    ( model |> incrementIP , Cmd.none )
                Write operand ->
                    case operand of
                        Constant _ ->
                            -- For a constant operand, writing doesn't make sense.
                            ( { model | instructionPointer = model.instructionPointer + 1 }, Cmd.none)

                        Direct regIndex ->
                            let
                                value = getRegisterValue regIndex model
                                updatedOutputTape = Array.set model.outputTapePointer (Just value) model.outputTape
                            in
                            (
                            { model
                                | outputTape = updatedOutputTape
                                , outputTapePointer = model.outputTapePointer + 1
                                , instructionPointer = model.instructionPointer + 1
                            }
                            , Cmd.none
                            )

                        Indirect regIndex ->
                            let
                                pointer = getRegisterValue regIndex model
                                value = getRegisterValue pointer model
                                updatedOutputTape = Array.set model.outputTapePointer (Just value) model.outputTape
                            in
                            (
                            { model
                                | outputTape = updatedOutputTape
                                , outputTapePointer = model.outputTapePointer + 1
                                , instructionPointer = model.instructionPointer + 1
                            }
                            , Cmd.none
                            )
