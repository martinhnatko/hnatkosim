module Ram.Utils.ExecuteInstruction exposing (..)

import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Instructions exposing (Instruction(..))
import Ram.Types.Operand exposing (Operand(..))
import Ram.Utils.HelperFunctions exposing (..)

import Array
import Dict
import Task
import Process

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
            let 
                dontHighlight : Bool
                dontHighlight = model.speedIdx == List.length ( Array.toList model.speeds ) && model.isRunning
            in
            case instr of
                
                -- LOAD: Update register 0 (accumulator) with the operand value.
                Load operand ->
                    case operand of
                        Constant n ->
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 n model.registers
                                }
                                , Cmd.none
                                )
                            else
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 n model.registers
                                    , highlighted_registers = Dict.insert 0 "bg-blue-200" model.highlighted_registers 
                                }
                                , Task.perform (\_ -> RemoveHighlightFromRegisters 0) (Process.sleep (toFloat highlightDuration))
                                )

                        Direct regIndex ->
                            let
                                value = getRegisterValue regIndex model

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 value model.registers
                                        , highlighted_registers = Dict.insert regIndex "bg-blue-200" model.highlighted_registers 
                                    }
                                
                                -- After `highlightDuration`, switch the highlight from the source register to the accumulator.
                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, regIndex) (1, 0, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                -- After an additional delay, remove the highlight from the accumulator.
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat (highlightDuration)))

                            in
                            if dontHighlight then
                                ( { model
                                     | instructionPointer = nextInstructionPointer
                                     , registers = Dict.insert 0 value model.registers
                                  }
                                , Cmd.none )
                            else
                                ( updatedModel, Cmd.batch [ switchHighlightCmd, removeHighlightCmd ] )

                        Indirect regIndex ->
                            let
                                pointer = getRegisterValue regIndex model
                                value = getRegisterValue pointer model

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 value model.registers
                                        , highlighted_registers = Dict.insert pointer "bg-blue-200" model.highlighted_registers 
                                    }

                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, pointer) (1, 0, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat (highlightDuration)))
                            in
                            if dontHighlight then
                                ( { model
                                     | instructionPointer = nextInstructionPointer
                                     , registers = Dict.insert 0 value model.registers
                                  }
                                , Cmd.none )
                            else
                                ( updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                -- STORE: Store the accumulator (register 0) into the given register.
                Store operand ->
                    case operand of
                        Constant _ ->
                            -- A STORE with a constant operand doesn't make sense; do nothing.
                            let
                                updatedModel =
                                    { model | instructionPointer = nextInstructionPointer }
                            in
                            (
                            updatedModel
                            , Cmd.none 
                            )

                        Direct regIndex ->
                            let
                                accVal = getRegisterValue 0 model

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert regIndex accVal model.registers
                                        , highlighted_registers = Dict.insert 0 "bg-blue-200" model.highlighted_registers 
                                    }
                                
                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, 0) (1, regIndex, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters regIndex)
                                        (Process.sleep (toFloat (highlightDuration)))
                            in
                            if dontHighlight then
                                ( { model
                                     | instructionPointer = nextInstructionPointer
                                     , registers = Dict.insert regIndex accVal model.registers
                                  }
                                , Cmd.none )
                            else
                                ( updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                        Indirect regIndex ->
                            let
                                pointer = getRegisterValue regIndex model
                                accVal = getRegisterValue 0 model

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert pointer accVal model.registers
                                        , highlighted_registers = Dict.insert 0 "bg-blue-200" model.highlighted_registers 
                                    }
                                
                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, 0) (1, pointer, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters pointer)
                                        (Process.sleep (toFloat (highlightDuration)))
                            in
                            if dontHighlight then
                                ( { model
                                     | instructionPointer = nextInstructionPointer
                                     , registers = Dict.insert pointer accVal model.registers
                                  }
                                , Cmd.none )
                            else
                                ( updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                -- ADD: Add the operand value to the accumulator.
                Add operand ->    
                    case operand of
                        Constant n ->
                            let
                                accVal = getRegisterValue 0 model
                                value = n

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 (accVal + value) model.registers
                                        , highlighted_registers = Dict.insert 0 "bg-blue-200" model.highlighted_registers 
                                    }
                                
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal + value) model.registers
                                  }
                                , Cmd.none
                                )
                            else
                                ( updatedModel
                                , removeHighlightCmd
                                )

                        Direct regIndex ->
                            let
                                accVal = getRegisterValue 0 model
                                value = getRegisterValue regIndex model

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 (accVal + value) model.registers
                                        , highlighted_registers = Dict.insert regIndex "bg-blue-200" model.highlighted_registers 
                                    }
                                
                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, regIndex) (1, 0, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                removeHighlightCmd =    
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal + value) model.registers
                                  }
                                , Cmd.none
                                )
                            else
                                ( updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                        Indirect regIndex ->
                            let
                                accVal = getRegisterValue 0 model
                                pointer = getRegisterValue regIndex model
                                value = getRegisterValue pointer model

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 (accVal + value) model.registers
                                        , highlighted_registers = Dict.insert pointer "bg-blue-200" model.highlighted_registers 
                                    }

                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, pointer) (1, 0, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))

                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal + value) model.registers
                                  }
                                , Cmd.none
                                )
                            else
                                (
                                updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                -- SUB: Subtract the operand value from the accumulator.
                Sub operand ->
                    case operand of
                        Constant n ->
                            let
                                accVal = getRegisterValue 0 model
                                value = n

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 (accVal - value) model.registers
                                        , highlighted_registers = Dict.insert 0 "bg-blue-200" model.highlighted_registers 
                                    }
                                
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal - value) model.registers
                                  }
                                , Cmd.none
                                )
                            else
                                ( updatedModel
                                , removeHighlightCmd
                                )

                        Direct regIndex ->
                            let
                                accVal = getRegisterValue 0 model
                                value = getRegisterValue regIndex model

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 (accVal - value) model.registers
                                        , highlighted_registers = Dict.insert regIndex "bg-blue-200" model.highlighted_registers 
                                    }
                                
                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, regIndex) (1, 0, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                removeHighlightCmd =    
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal - value) model.registers
                                  }
                                , Cmd.none
                                )
                            else
                                ( updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                        Indirect regIndex ->
                            let
                                accVal = getRegisterValue 0 model
                                pointer = getRegisterValue regIndex model
                                value = getRegisterValue pointer model

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 (accVal - value) model.registers
                                        , highlighted_registers = Dict.insert pointer "bg-blue-200" model.highlighted_registers 
                                    }

                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, pointer) (1, 0, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))

                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal - value) model.registers
                                  }
                                , Cmd.none
                                )
                            else
                                (
                                updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                -- MUL: Multiply the accumulator by the operand.
                Mul operand ->
                    case operand of
                        Constant n ->
                            let
                                accVal = getRegisterValue 0 model
                                value = n

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 (accVal * value) model.registers
                                        , highlighted_registers = Dict.insert 0 "bg-blue-200" model.highlighted_registers 
                                    }
                                
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal * value) model.registers
                                  }
                                , Cmd.none
                                )
                            else
                                ( updatedModel
                                , removeHighlightCmd
                                )

                        Direct regIndex ->
                            let
                                accVal = getRegisterValue 0 model
                                value = getRegisterValue regIndex model

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 (accVal * value) model.registers
                                        , highlighted_registers = Dict.insert regIndex "bg-blue-200" model.highlighted_registers 
                                    }
                                
                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, regIndex) (1, 0, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                removeHighlightCmd =    
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal * value) model.registers
                                  }
                                , Cmd.none
                                )
                            else
                                ( updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                        Indirect regIndex ->
                            let
                                accVal = getRegisterValue 0 model
                                pointer = getRegisterValue regIndex model
                                value = getRegisterValue pointer model

                                updatedModel =
                                    { model
                                        | instructionPointer = nextInstructionPointer
                                        , registers = Dict.insert 0 (accVal * value) model.registers
                                        , highlighted_registers = Dict.insert pointer "bg-blue-200" model.highlighted_registers 
                                    }

                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, pointer) (1, 0, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))

                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal * value) model.registers
                                  }
                                , Cmd.none
                                )
                            else
                                (
                                updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                -- DIV: Divide the accumulator by the operand, if nonzero.
                Div operand ->
                    case operand of
                        Constant n ->
                            let
                                accVal = getRegisterValue 0 model
                                value = n

                                updatedModel =
                                    if value /= 0 then
                                        { model
                                            | instructionPointer = nextInstructionPointer
                                            , registers = Dict.insert 0 (accVal // value) model.registers
                                            , highlighted_registers = Dict.insert 0 "bg-blue-200" model.highlighted_registers 
                                        }
                                    else
                                        {
                                            model | instructionPointer = nextInstructionPointer
                                            , highlighted_registers = Dict.insert 0 "bg-red-200" model.highlighted_registers
                                        }
                                
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))

                            in
                            if dontHighlight && value /= 0 then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal // value) model.registers
                                  }
                                , Cmd.none
                                )
                            else if dontHighlight && value == 0 then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                  }
                                , Cmd.none
                                )
                            else
                                (
                                updatedModel
                                , removeHighlightCmd
                                )

                        Direct regIndex ->
                            let
                                accVal = getRegisterValue 0 model
                                value = getRegisterValue regIndex model

                                updatedModel =
                                    if value /= 0 then
                                        { model
                                            | instructionPointer = nextInstructionPointer
                                            , registers = Dict.insert 0 (accVal // value) model.registers
                                            , highlighted_registers = Dict.insert regIndex "bg-blue-200" model.highlighted_registers 
                                        }
                                    else
                                        {
                                            model | instructionPointer = nextInstructionPointer
                                            , highlighted_registers = Dict.insert regIndex "bg-red-200" model.highlighted_registers
                                        }
                                
                                switchHighlightCmd =
                                    if value /= 0 then
                                        Task.perform (\_ -> SwitchHighlight (1, regIndex) (1, 0, "bg-blue-200"))
                                            (Process.sleep (toFloat (highlightDuration // 2)))
                                    else
                                        Task.perform (\_ -> SwitchHighlight (1, regIndex) (1, 0, "bg-red-200"))
                                            (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                removeHighlightCmd =    
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))

                            in
                            if dontHighlight && value /= 0 then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal // value) model.registers
                                  }
                                , Cmd.none
                                )
                            else if dontHighlight && value == 0 then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                  }
                                , Cmd.none
                                )
                            else
                                (
                                updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                        Indirect regIndex ->
                            let
                                accVal = getRegisterValue 0 model
                                pointer = getRegisterValue regIndex model
                                value = getRegisterValue pointer model

                                updatedModel =
                                    if value /= 0 then
                                        { model
                                            | instructionPointer = nextInstructionPointer
                                            , registers = Dict.insert 0 (accVal // value) model.registers
                                            , highlighted_registers = Dict.insert pointer "bg-blue-200" model.highlighted_registers 
                                        }
                                    else
                                        {
                                            model | instructionPointer = nextInstructionPointer
                                            , highlighted_registers = Dict.insert pointer "bg-red-200" model.highlighted_registers
                                        }
                                
                                switchHighlightCmd =
                                    if value /= 0 then
                                        Task.perform (\_ -> SwitchHighlight (1, pointer) (1, 0, "bg-blue-200"))
                                            (Process.sleep (toFloat (highlightDuration // 2)))
                                    else
                                        Task.perform (\_ -> SwitchHighlight (1, pointer) (1, 0, "bg-red-200"))
                                            (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight && value /= 0 then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , registers = Dict.insert 0 (accVal // value) model.registers
                                  }
                                , Cmd.none
                                )
                            else if dontHighlight && value == 0 then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                  }
                                , Cmd.none
                                )
                            else
                                (
                                updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
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
                        ( { model | instructionPointer = nextInstructionPointer }
                        , Cmd.none 
                        )

                -- JGTZ: Jump if the accumulator is greater than zero.
                Jgtz idx _ ->
                    if getRegisterValue 0 model > 0 then
                        ( { model | instructionPointer = idx }
                        , Cmd.none 
                        )
                    else
                        ( { model | instructionPointer = nextInstructionPointer }
                        , Cmd.none 
                        )

                -- HALT: End the program.
                Halt ->
                    ( { model | halted = True, isRunning = False }
                    , Cmd.none 
                    )

                -- LABEL: Do nothing (labels are stored in model.labels).
                Label _ ->
                    ( { model | instructionPointer = nextInstructionPointer }
                    , Cmd.none 
                    )

                UnknownInstruction ->
                    ( { model | instructionPointer = nextInstructionPointer }
                    , Cmd.none 
                    )

                Read operand ->
                    case operand of
                        Constant _ ->
                            ( { model
                                | instructionPointer = nextInstructionPointer
                              }
                            , Cmd.none
                            )

                        Direct regIndex ->
                            case Array.get model.inputTapePointer model.inputTape of
                                Just val ->
                                    let
                                        newTapePointer = model.inputTapePointer + 1
                                        
                                        updatedModel = { model
                                                        | instructionPointer = nextInstructionPointer
                                                        , inputTapePointer = newTapePointer
                                                        , registers = Dict.insert regIndex val model.registers
                                                        , highlighted_input_tape = Dict.insert model.inputTapePointer "bg-blue-200" model.highlighted_input_tape
                                                       }
                                        
                                        switchHighlightCmd =
                                            Task.perform (\_ -> SwitchHighlight (0, model.inputTapePointer) (1, regIndex, "bg-blue-200"))
                                                (Process.sleep (toFloat (highlightDuration // 2)))
                                        
                                        removeHighlightCmd =
                                            Task.perform (\_ -> RemoveHighlightFromRegisters regIndex)
                                                (Process.sleep (toFloat highlightDuration))
   
                                            
                                    in
                                    if dontHighlight then
                                        ( { model
                                            | instructionPointer = nextInstructionPointer
                                            , inputTapePointer = newTapePointer
                                            , registers = Dict.insert regIndex val model.registers
                                          }
                                        , Cmd.none
                                        )
                                    else
                                        ( updatedModel
                                        , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                        )

                                _ ->
                                    -- ERRORRRRRRRRRRRRRRRRRRR
                                    ( { model
                                        | instructionPointer = nextInstructionPointer
                                      }
                                    , Cmd.none )

                        Indirect regIndex ->
                            let
                                pointer = getRegisterValue regIndex model
                            in
                            case Array.get model.inputTapePointer model.inputTape of
                                Just val ->
                                    let
                                        newTapePointer = model.inputTapePointer + 1
                                        
                                        updatedModel = { model
                                                        | instructionPointer = nextInstructionPointer
                                                        , inputTapePointer = newTapePointer
                                                        , registers = Dict.insert pointer val model.registers
                                                        , highlighted_input_tape = Dict.insert model.inputTapePointer "bg-blue-200" model.highlighted_input_tape
                                                       }
                                        switchHighlightCmd =
                                            Task.perform (\_ -> SwitchHighlight (0, model.inputTapePointer) (1, pointer, "bg-blue-200"))
                                                (Process.sleep (toFloat (highlightDuration // 2)))
                                        
                                        removeHighlightCmd =
                                            Task.perform (\_ -> RemoveHighlightFromRegisters pointer)
                                                (Process.sleep (toFloat highlightDuration))

                                    in
                                    if dontHighlight then
                                        ( { model
                                            | instructionPointer = nextInstructionPointer
                                            , inputTapePointer = newTapePointer
                                            , registers = Dict.insert pointer val model.registers
                                          }
                                        , Cmd.none
                                        )
                                    else
                                        ( updatedModel
                                        , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                        )

                                _ ->
                                    -- ERRORRRRRRRRRRRRRRRRRRR
                                    ( {
                                        model | instructionPointer = nextInstructionPointer
                                      }
                                    , Cmd.none )
                Write operand ->
                    case operand of
                        Constant _ ->
                            -- skip for constant
                            ( { model | instructionPointer = nextInstructionPointer }, Cmd.none)

                        Direct regIndex ->
                            let
                                value = getRegisterValue regIndex model
                                updatedOutputTape = Array.push value model.outputTape
                                lastIdx = List.length (Array.toList (updatedOutputTape)) - 1

                                updatedModel = { model
                                                | instructionPointer = nextInstructionPointer
                                                , outputTape = updatedOutputTape
                                                , highlighted_registers = Dict.insert regIndex "bg-blue-200" model.highlighted_registers
                                               }

                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, regIndex) (2, lastIdx, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromOutputTape lastIdx)
                                        (Process.sleep (toFloat highlightDuration))

                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , outputTape = updatedOutputTape
                                  }
                                , Cmd.none
                                )
                            else
                                ( updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )

                        Indirect regIndex ->
                            let
                                pointer = getRegisterValue regIndex model
                                value = getRegisterValue pointer model
                                updatedOutputTape = Array.push value model.outputTape
                                lastIdx = List.length (Array.toList (updatedOutputTape)) - 1

                                updatedModel = { model
                                                | instructionPointer = nextInstructionPointer
                                                , outputTape = updatedOutputTape
                                                , highlighted_registers = Dict.insert pointer "bg-blue-200" model.highlighted_registers
                                               }
                                
                                switchHighlightCmd =
                                    Task.perform (\_ -> SwitchHighlight (1, pointer) (2, lastIdx, "bg-blue-200"))
                                        (Process.sleep (toFloat (highlightDuration // 2)))
                                
                                removeHighlightCmd =
                                    Task.perform (\_ -> RemoveHighlightFromOutputTape lastIdx)
                                        (Process.sleep (toFloat highlightDuration))
                            in
                            if dontHighlight then
                                ( { model
                                    | instructionPointer = nextInstructionPointer
                                    , outputTape = updatedOutputTape
                                  }
                                , Cmd.none
                                )
                            else
                                (
                                updatedModel
                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                )
