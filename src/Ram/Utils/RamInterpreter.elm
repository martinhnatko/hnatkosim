module Ram.Utils.RamInterpreter exposing (..)

import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Instructions exposing (Instruction(..), Operand(..))
import Ram.Types.ErrorType exposing (ErrorType(..))

import Ram.Utils.HelperFunctions exposing (..)

import Shared.Components.Console exposing (ConsoleMessageType(..))

import Array
import Dict
import Task
import Process
import Time
import Dict exposing (Dict)

runAllInstructions : (Model, Cmd Msg, Int) -> (Model, Cmd Msg, Int)
runAllInstructions (model, someCmd, numOfMsgs) =
    if model.executedInstructions >= model.totalMaxExecutedInstructions then
        (model, someCmd, numOfMsgs)
    else
        if (model.instructionPointer >= List.length model.instructions) || model.halted then
            (model, someCmd, numOfMsgs)
        else
            if numOfMsgs >= 100 then
                ( { model | tooManyRuntimeMsgs = True }, someCmd, numOfMsgs )
            else
                let
                    (nextModel, someNewCmd) = executeInstruction model 0
                in
                if someNewCmd == Cmd.none then
                    runAllInstructions (nextModel, someCmd, numOfMsgs)
                else
                    runAllInstructions (nextModel, Cmd.batch [ someCmd, someNewCmd ], numOfMsgs + 1)

calculateLogTime : Int -> Int -> Int
calculateLogTime base value =
    let
        valueAbsolute = abs value
    in
    if valueAbsolute <= 0 then
        1
    else
        floor ( logBase (toFloat base) (toFloat valueAbsolute) ) + 1

updateRegisterAndComputeLogSpace : Int -> Int -> Int -> Dict Int (Int, Maybe Int) -> (Dict Int (Int, Maybe Int), Int)
updateRegisterAndComputeLogSpace regIdx newVal logBase registers =
    let
        ( oldVal, maybeOldMax ) =
            Dict.get regIdx registers
                |> Maybe.withDefault (0, Nothing)

        oldMaxVal =
            Maybe.withDefault (abs oldVal) maybeOldMax

        newMaxVal =
            max oldMaxVal (abs newVal)

        updatedRegisters =
            Dict.insert regIdx (newVal, Just newMaxVal) registers

        newLogSpace =
            Dict.foldl
                (\_ (_, maybeM) acc ->
                    case maybeM of
                        Just maxVal ->
                            acc + calculateLogTime logBase maxVal

                        Nothing ->
                            acc
                )
                0
                updatedRegisters
    in
    ( updatedRegisters, newLogSpace )

getRegisterValue : Int -> Model -> Maybe Int
getRegisterValue regIndex model =
    Dict.get regIndex model.registers |> Maybe.map Tuple.first

executeInstruction : Model -> Int -> (Model, Cmd Msg)
executeInstruction model highlightDuration =
    let
        currentInstruction =
            List.head (List.drop model.instructionPointer model.instructions)

        nextInstructionPointer =
            model.instructionPointer + 1
        
        updateInstructionAt index newInstruction instructions =
            List.indexedMap (\i instr -> if i == index then newInstruction else instr) instructions

    in
    case currentInstruction of
        Nothing ->
            -- No more instructions to execute, stop the simulator
            ( { model | halted = True, isRunning = False }, Cmd.none )

        Just instr ->
            let 
                dontHighlight : Bool
                dontHighlight = ( model.speedIdx == 7 || model.speedIdx == 6 ) && model.isRunning
            in
            case instr of
                
                -- LOAD: Update register 0 (accumulator) with the operand value.
                Load operand isError exeCount ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }
                            , Cmd.none 
                            )
                        _ ->
                            case operand of
                                Constant n ->
                                    let
                                        newLogTime = model.logTime + calculateLogTime model.logBase n
                                        
                                        (newRegisters, newLogSpace) = updateRegisterAndComputeLogSpace 0 n model.logBase model.registers
                                    in
                                    if dontHighlight then
                                        ( { model
                                            | instructionPointer = nextInstructionPointer
                                            , registers = newRegisters
                                            , executedInstructions = model.executedInstructions + 1
                                            , instructions = updateInstructionAt model.instructionPointer (Load (Constant n) Nothing (exeCount + 1)) model.instructions
                                            
                                            , logTime = newLogTime
                                            , logSpace = newLogSpace
                                        }
                                        , Cmd.none
                                        )
                                    else
                                        ( { model
                                            | instructionPointer = nextInstructionPointer
                                            , registers = newRegisters
                                            , executedInstructions = model.executedInstructions + 1
                                            , instructions = updateInstructionAt model.instructionPointer (Load (Constant n) Nothing (exeCount + 1)) model.instructions
                                            
                                            , logTime = newLogTime
                                            , logSpace = newLogSpace
                                            
                                            , highlighted_registers = Dict.insert 0 "bg-blue-200" Dict.empty
                                        }
                                        , Task.perform (\_ -> RemoveHighlightFromRegisters 0) (Process.sleep (toFloat highlightDuration))
                                        )

                                Direct regIndex ->
                                    let
                                        maybeValue = getRegisterValue regIndex model
                                    in
                                    case maybeValue of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        
                                        Just value ->
                                            let
                                                newLogTime = model.logTime + calculateLogTime model.logBase value + calculateLogTime model.logBase regIndex
                                                ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 value model.logBase model.registers
                                            in
                                            if dontHighlight then
                                                ( { model
                                                        | instructionPointer = nextInstructionPointer
                                                        , registers = newRegisters
                                                        , executedInstructions = model.executedInstructions + 1
                                                        , instructions = updateInstructionAt model.instructionPointer (Load (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                        
                                                        , logTime = newLogTime
                                                        , logSpace = newLogSpace
                                                    }
                                                , Cmd.none )
                                            else
                                                let
                                                    updatedModel =
                                                        { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Load (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                            , logSpace = newLogSpace
                                                            
                                                            , highlighted_registers = Dict.insert regIndex "bg-blue-200" Dict.empty
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
                                                ( updatedModel, Cmd.batch [ switchHighlightCmd, removeHighlightCmd ] )

                                Indirect regIndex ->
                                    let
                                        maybePointer = getRegisterValue regIndex model
                                    in
                                    case maybePointer of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just pointer ->
                                            let
                                                maybeValue = getRegisterValue pointer model
                                            in
                                            case maybeValue of
                                                Nothing ->
                                                    -- runtime error - accessing non-existent register
                                                    ( 
                                                        { model 
                                                            | instructionPointer = nextInstructionPointer
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Load (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = model.logTime + calculateLogTime model.logBase (maybeValue |> Maybe.withDefault 0 ) + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        }
                                                        , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to access a non-existent register.")
                                                    )
                                                Just value ->
                                                    let
                                                        newLogTime = model.logTime + calculateLogTime model.logBase value + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 value model.logBase model.registers
                                                    in
                                                    if dontHighlight then
                                                        ( { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Load (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                            , logSpace = newLogSpace
                                                        }
                                                        , Cmd.none )
                                                    else
                                                        let
                                                            updatedModel =
                                                                { model
                                                                    | instructionPointer = nextInstructionPointer
                                                                    , registers = newRegisters
                                                                    , executedInstructions = model.executedInstructions + 1
                                                                    , instructions = updateInstructionAt model.instructionPointer (Load (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                    
                                                                    , logTime = newLogTime
                                                                    , logSpace = newLogSpace

                                                                    , highlighted_registers = Dict.insert pointer "bg-blue-200" Dict.empty
                                                                }

                                                            switchHighlightCmd =
                                                                Task.perform (\_ -> SwitchHighlight (1, pointer) (1, 0, "bg-blue-200"))
                                                                    (Process.sleep (toFloat (highlightDuration // 2)))
                                                            
                                                            removeHighlightCmd =
                                                                Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                                    (Process.sleep (toFloat (highlightDuration)))
                                                        in
                                                        ( updatedModel
                                                        , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                        )

                -- STORE: Store the accumulator (register 0) into the given register.
                Store operand isError exeCount ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }
                            , Cmd.none 
                            )
                        _ ->
                            case operand of
                                Constant _ ->
                                    -- error detected by parsing
                                    (
                                    { model | instructionPointer = nextInstructionPointer }
                                    , Cmd.none 
                                    )

                                Direct regIndex ->
                                    let
                                        maybeAccVal = getRegisterValue 0 model
                                    in
                                    case maybeAccVal of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just accVal ->
                                            let
                                                newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase regIndex
                                                ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace regIndex accVal model.logBase model.registers
                                            in
                                            if dontHighlight then
                                                ( { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , registers = newRegisters
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Store (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logTime = newLogTime
                                                    , logSpace = newLogSpace
                                                }
                                                , Cmd.none )
                                            else
                                                let
                                                    updatedModel =
                                                        { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Store (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                            , logSpace = newLogSpace

                                                            , highlighted_registers = Dict.insert 0 "bg-blue-200" Dict.empty
                                                        }
                                                    
                                                    switchHighlightCmd =
                                                        Task.perform (\_ -> SwitchHighlight (1, 0) (1, regIndex, "bg-blue-200"))
                                                            (Process.sleep (toFloat (highlightDuration // 2)))
                                                    
                                                    removeHighlightCmd =
                                                        Task.perform (\_ -> RemoveHighlightFromRegisters regIndex)
                                                            (Process.sleep (toFloat (highlightDuration)))
                                                in
                                                ( updatedModel
                                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                )

                                Indirect regIndex ->
                                    let
                                        maybePointer = getRegisterValue regIndex model
                                    in
                                    case maybePointer of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just pointer ->
                                            let
                                                maybeValue = getRegisterValue pointer model
                                                accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                            in
                                            case maybeValue of
                                                Nothing ->
                                                    -- ERRORRRRRRRRRRRRRRRRRRR
                                                    ( 
                                                        { model 
                                                            | instructionPointer = nextInstructionPointer
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Store (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        }
                                                        , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to access a non-existent register.") 
                                                    )
                                                Just _ ->
                                                    let
                                                        newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace pointer accVal model.logBase model.registers
                                                    in
                                                    if dontHighlight then
                                                        ( 
                                                        { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Store (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                            , logSpace = newLogSpace
                                                        }
                                                        , Cmd.none )
                                                    else
                                                        let
                                                            updatedModel =
                                                                { model
                                                                    | instructionPointer = nextInstructionPointer
                                                                    , registers = newRegisters
                                                                    , executedInstructions = model.executedInstructions + 1
                                                                    , instructions = updateInstructionAt model.instructionPointer (Store (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                    
                                                                    , logTime = newLogTime
                                                                    , logSpace = newLogSpace

                                                                    , highlighted_registers = Dict.insert 0 "bg-blue-200" Dict.empty
                                                                }
                                                            
                                                            switchHighlightCmd =
                                                                Task.perform (\_ -> SwitchHighlight (1, 0) (1, pointer, "bg-blue-200"))
                                                                    (Process.sleep (toFloat (highlightDuration // 2)))
                                                            
                                                            removeHighlightCmd =
                                                                Task.perform (\_ -> RemoveHighlightFromRegisters pointer)
                                                                    (Process.sleep (toFloat (highlightDuration)))
                                                        in
                                                        ( updatedModel
                                                        , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                        )

                -- ADD: Add the operand value to the accumulator.
                Add operand isError exeCount ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }
                            , Cmd.none 
                            )
                        _ ->    
                            case operand of
                                Constant value ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value
                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal + value) model.logBase model.registers

                                    in
                                    if dontHighlight then
                                        ( { model
                                            | instructionPointer = nextInstructionPointer
                                            , registers = newRegisters
                                            , executedInstructions = model.executedInstructions + 1
                                            , instructions = updateInstructionAt model.instructionPointer (Add (Constant value) Nothing (exeCount + 1)) model.instructions
                                            
                                            , logTime = newLogTime
                                            , logSpace = newLogSpace
                                        }
                                        , Cmd.none
                                        )
                                    else
                                        let

                                            updatedModel =
                                                { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , registers = newRegisters
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Add (Constant value) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logTime = newLogTime
                                                    , logSpace = newLogSpace
                                                    
                                                    , highlighted_registers = Dict.insert 0 "bg-blue-200" Dict.empty
                                                }
                                            
                                            removeHighlightCmd =
                                                Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                    (Process.sleep (toFloat highlightDuration))
                                        in
                                        ( updatedModel
                                        , removeHighlightCmd
                                        )

                                Direct regIndex ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        maybeValue = getRegisterValue regIndex model
                                    in
                                    case maybeValue of
                                        Nothing ->
                                            -- Parsing error
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just value ->
                                            let
                                                newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value + calculateLogTime model.logBase regIndex
                                                ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal + value) model.logBase model.registers
                                            in
                                            if dontHighlight then
                                                ( { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , registers = newRegisters
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Add (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logTime = newLogTime
                                                    , logSpace = newLogSpace
                                                }
                                                , Cmd.none
                                                )
                                            else
                                                let
                                                    updatedModel =
                                                        { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Add (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                            , logSpace = newLogSpace

                                                            , highlighted_registers = Dict.insert regIndex "bg-blue-200" Dict.empty
                                                        }
                                                    
                                                    switchHighlightCmd =
                                                        Task.perform (\_ -> SwitchHighlight (1, regIndex) (1, 0, "bg-blue-200"))
                                                            (Process.sleep (toFloat (highlightDuration // 2)))
                                                    
                                                    removeHighlightCmd =    
                                                        Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                            (Process.sleep (toFloat highlightDuration))
                                                in
                                                ( updatedModel
                                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                )

                                Indirect regIndex ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        maybePointer = getRegisterValue regIndex model
                                    in
                                    case maybePointer of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just pointer ->
                                            let
                                                maybeValue = getRegisterValue pointer model
                                            in
                                            case maybeValue of
                                                Nothing ->
                                                    -- ERRORRRRRRRRRRRRRRRRRRR
                                                    ( 
                                                        { model 
                                                            | instructionPointer = nextInstructionPointer
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Add (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase ( maybeValue |> Maybe.withDefault 0 ) + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        }
                                                        , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to access a non-existent register.")
                                                    )
                                                Just value ->
                                                    let
                                                        newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal + value) model.logBase model.registers
                                                    in
                                                    if dontHighlight then
                                                        ( { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Add (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                            , logSpace = newLogSpace
                                                        }
                                                        , Cmd.none
                                                        )
                                                    else
                                                        let
                                                            updatedModel =
                                                                { model
                                                                    | instructionPointer = nextInstructionPointer
                                                                    , registers = newRegisters
                                                                    , executedInstructions = model.executedInstructions + 1
                                                                    , instructions = updateInstructionAt model.instructionPointer (Add (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                    
                                                                    , logTime = newLogTime
                                                                    , logSpace = newLogSpace
                                                                    
                                                                    , highlighted_registers = Dict.insert pointer "bg-blue-200" Dict.empty
                                                                }

                                                            switchHighlightCmd =
                                                                Task.perform (\_ -> SwitchHighlight (1, pointer) (1, 0, "bg-blue-200"))
                                                                    (Process.sleep (toFloat (highlightDuration // 2)))

                                                            removeHighlightCmd =
                                                                Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                                    (Process.sleep (toFloat highlightDuration))
                                                        in
                                                        (
                                                        updatedModel
                                                        , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                        )

                -- SUB: Subtract the operand value from the accumulator.
                Sub operand isError exeCount ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }
                            , Cmd.none 
                            )
                        _ ->
                            case operand of
                                Constant value ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value
                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal - value) model.logBase model.registers
                                    in
                                    if dontHighlight then
                                        ( { model
                                            | instructionPointer = nextInstructionPointer
                                            , registers = newRegisters
                                            , executedInstructions = model.executedInstructions + 1
                                            , instructions = updateInstructionAt model.instructionPointer (Sub (Constant value) Nothing (exeCount + 1)) model.instructions
                                            
                                            , logTime = newLogTime
                                            , logSpace = newLogSpace
                                        }
                                        , Cmd.none
                                        )
                                    else
                                        let
                                            updatedModel =
                                                { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , registers = newRegisters
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Sub (Constant value) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logTime = newLogTime
                                                    , logSpace = newLogSpace
                                                    
                                                    , highlighted_registers = Dict.insert 0 "bg-blue-200" Dict.empty
                                                }
                                            
                                            removeHighlightCmd =
                                                Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                    (Process.sleep (toFloat highlightDuration))
                                        in
                                        ( updatedModel
                                        , removeHighlightCmd
                                        )

                                Direct regIndex ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        maybeValue = getRegisterValue regIndex model
                                    in
                                    case maybeValue of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just value ->
                                            let
                                                newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value + calculateLogTime model.logBase regIndex
                                                ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal - value) model.logBase model.registers
                                            in
                                            if dontHighlight then
                                                ( { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , registers = newRegisters
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Sub (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logSpace = newLogSpace
                                                    , logTime = newLogTime
                                                }
                                                , Cmd.none
                                                )
                                            else
                                                let
                                                    updatedModel =
                                                        { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Sub (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                            , logSpace = newLogSpace
                                                            
                                                            , highlighted_registers = Dict.insert regIndex "bg-blue-200" Dict.empty
                                                        }
                                                    
                                                    switchHighlightCmd =
                                                        Task.perform (\_ -> SwitchHighlight (1, regIndex) (1, 0, "bg-blue-200"))
                                                            (Process.sleep (toFloat (highlightDuration // 2)))
                                                    
                                                    removeHighlightCmd =    
                                                        Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                            (Process.sleep (toFloat highlightDuration))
                                                in
                                                ( updatedModel
                                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                )

                                Indirect regIndex ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        maybePointer = getRegisterValue regIndex model
                                    in
                                    case maybePointer of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just pointer ->
                                            let
                                                maybeValue = getRegisterValue pointer model
                                            in
                                            case maybeValue of
                                                Nothing ->
                                                    -- ERRORRRRRRRRRRRRRRRRRRR
                                                    ( 
                                                        { model 
                                                            | instructionPointer = nextInstructionPointer
                                                            , instructions = updateInstructionAt model.instructionPointer (Sub (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            , executedInstructions = model.executedInstructions + 1
                                                            
                                                            , logTime =  model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase (maybeValue |> Maybe.withDefault 0)+ calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        }
                                                        , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to access a non-existent register.")
                                                    )
                                                Just value ->
                                                    let
                                                        newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal - value) model.logBase model.registers
                                                    in
                                                    if dontHighlight then
                                                        ( { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Sub (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logSpace = newLogSpace
                                                            , logTime = newLogTime
                                                        }
                                                        , Cmd.none
                                                        )
                                                    else
                                                        let
                                                            updatedModel =
                                                                { model
                                                                    | instructionPointer = nextInstructionPointer
                                                                    , registers = newRegisters
                                                                    , executedInstructions = model.executedInstructions + 1
                                                                    , instructions = updateInstructionAt model.instructionPointer (Sub (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                    
                                                                    , logTime = newLogTime
                                                                    , logSpace = newLogSpace
                                                                    
                                                                    , highlighted_registers = Dict.insert pointer "bg-blue-200" Dict.empty
                                                                }

                                                            switchHighlightCmd =
                                                                Task.perform (\_ -> SwitchHighlight (1, pointer) (1, 0, "bg-blue-200"))
                                                                    (Process.sleep (toFloat (highlightDuration // 2)))

                                                            removeHighlightCmd =
                                                                Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                                    (Process.sleep (toFloat highlightDuration))
                                                        in
                                                        (
                                                        updatedModel
                                                        , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                        )

                -- MUL: Multiply the accumulator by the operand.
                Mul operand isError exeCount ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }
                            , Cmd.none 
                            )
                        _ ->
                            case operand of
                                Constant value ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value
                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal * value) model.logBase model.registers
                                    in
                                    if dontHighlight then
                                        ( { model
                                            | instructionPointer = nextInstructionPointer
                                            , registers = newRegisters
                                            , executedInstructions = model.executedInstructions + 1
                                            , instructions = updateInstructionAt model.instructionPointer (Mul (Constant value) Nothing (exeCount + 1)) model.instructions
                                            
                                            , logSpace = newLogSpace
                                            , logTime = newLogTime
                                        }
                                        , Cmd.none
                                        )
                                    else
                                        let
                                            updatedModel =
                                                { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , registers = newRegisters
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Mul (Constant value) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logTime = newLogTime
                                                    , logSpace = newLogSpace
                                                    
                                                    , highlighted_registers = Dict.insert 0 "bg-blue-200" Dict.empty
                                                }
                                            
                                            removeHighlightCmd =
                                                Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                    (Process.sleep (toFloat highlightDuration))
                                        in
                                        ( updatedModel
                                        , removeHighlightCmd
                                        )

                                Direct regIndex ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        maybeValue = getRegisterValue regIndex model
                                    in
                                    case maybeValue of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just value ->
                                            let
                                                newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value + calculateLogTime model.logBase regIndex
                                                ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal * value) model.logBase model.registers
                                            in
                                            if dontHighlight then
                                                ( { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , registers = newRegisters
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Mul (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logSpace = newLogSpace
                                                    , logTime = newLogTime
                                                }
                                                , Cmd.none
                                                )
                                            else
                                                let
                                                    updatedModel =
                                                        { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Mul (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                            , logSpace = newLogSpace
                                                            
                                                            , highlighted_registers = Dict.insert regIndex "bg-blue-200" Dict.empty
                                                        }
                                                    
                                                    switchHighlightCmd =
                                                        Task.perform (\_ -> SwitchHighlight (1, regIndex) (1, 0, "bg-blue-200"))
                                                            (Process.sleep (toFloat (highlightDuration // 2)))
                                                    
                                                    removeHighlightCmd =    
                                                        Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                            (Process.sleep (toFloat highlightDuration))
                                                in
                                                ( updatedModel
                                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                )

                                Indirect regIndex ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        maybePointer = getRegisterValue regIndex model
                                    in
                                    case maybePointer of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just pointer ->
                                            let
                                                maybeValue = getRegisterValue pointer model
                                            in
                                            case maybeValue of
                                                Nothing ->
                                                    -- ERRORRRRRRRRRRRRRRRRRRR
                                                    ( 
                                                        { model 
                                                            | instructionPointer = nextInstructionPointer
                                                            , instructions = updateInstructionAt model.instructionPointer (Mul (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            , executedInstructions = model.executedInstructions + 1
                                                            
                                                            , logTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase ( maybeValue |> Maybe.withDefault 0 ) + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        }
                                                        , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to access a non-existent register.")
                                                    )
                                                Just value ->
                                                    let
                                                        newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal * value) model.logBase model.registers
                                                    in
                                                    if dontHighlight then
                                                        ( { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Mul (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logSpace = newLogSpace
                                                            , logTime = newLogTime
                                                        }
                                                        , Cmd.none
                                                        )
                                                    else
                                                        let
                                                            updatedModel =
                                                                { model
                                                                    | instructionPointer = nextInstructionPointer
                                                                    , registers = newRegisters
                                                                    , executedInstructions = model.executedInstructions + 1
                                                                    , instructions = updateInstructionAt model.instructionPointer (Mul (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                    
                                                                    , logTime = newLogTime
                                                                    , logSpace = newLogSpace

                                                                    , highlighted_registers = Dict.insert pointer "bg-blue-200" Dict.empty 
                                                                }

                                                            switchHighlightCmd =
                                                                Task.perform (\_ -> SwitchHighlight (1, pointer) (1, 0, "bg-blue-200"))
                                                                    (Process.sleep (toFloat (highlightDuration // 2)))

                                                            removeHighlightCmd =
                                                                Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                                    (Process.sleep (toFloat highlightDuration))
                                                        in
                                                        (
                                                        updatedModel
                                                        , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                        )

                -- DIV: Divide the accumulator by the operand, if nonzero.
                Div operand isError exeCount ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }
                            , Cmd.none 
                            )
                        _ ->
                            case operand of
                                Constant value ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value
                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal // value) model.logBase model.registers
                                    in
                                    if dontHighlight then
                                        ( { model
                                            | instructionPointer = nextInstructionPointer
                                            , registers = newRegisters
                                            , executedInstructions = model.executedInstructions + 1
                                            , instructions = updateInstructionAt model.instructionPointer (Div (Constant value) Nothing (exeCount + 1)) model.instructions
                                            
                                            , logSpace = newLogSpace
                                            , logTime = newLogTime
                                        }
                                        , Cmd.none
                                        )
                                    else
                                        let
                                            updatedModel =
                                                { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , registers = newRegisters
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Div (Constant value) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logTime = newLogTime
                                                    , logSpace = newLogSpace
                                                    
                                                    , highlighted_registers = Dict.insert 0 "bg-blue-200" Dict.empty
                                                }
                                            
                                            removeHighlightCmd =
                                                Task.perform (\_ -> RemoveHighlightFromRegisters 0)
                                                    (Process.sleep (toFloat highlightDuration))

                                        in
                                        (
                                        updatedModel
                                        , removeHighlightCmd
                                        )

                                Direct regIndex ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        maybeValue = getRegisterValue regIndex model
                                    in
                                    case maybeValue of
                                        Nothing ->
                                            -- error detected by parsing
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just value ->
                                            let
                                                newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value + calculateLogTime model.logBase regIndex
                                                ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal // value) model.logBase model.registers
                                            in
                                            if dontHighlight && value /= 0 then
                                                ( { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , registers = newRegisters
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Div (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logSpace = newLogSpace
                                                    , logTime = newLogTime
                                                }
                                                , Cmd.none
                                                )
                                            else if dontHighlight && value == 0 then
                                                ( { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Div (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logTime = newLogTime
                                                }
                                                , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to divide by zero.")
                                                )
                                            else 
                                                let
                                                    updatedModel =
                                                        if value /= 0 then
                                                            { model
                                                                | instructionPointer = nextInstructionPointer
                                                                , registers = newRegisters
                                                                , executedInstructions = model.executedInstructions + 1
                                                                , instructions = updateInstructionAt model.instructionPointer (Div (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                                
                                                                , logTime = newLogTime
                                                                , logSpace = newLogSpace

                                                                , highlighted_registers = Dict.insert regIndex "bg-blue-200" Dict.empty
                                                            }
                                                        else
                                                            { model 
                                                                | instructionPointer = nextInstructionPointer
                                                                , executedInstructions = model.executedInstructions + 1
                                                                , instructions = updateInstructionAt model.instructionPointer (Div (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                                
                                                                , logTime = newLogTime
                                                                
                                                                , highlighted_registers = Dict.insert regIndex "bg-red-200" Dict.empty
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
                                                if value == 0 then
                                                    (
                                                    updatedModel
                                                    , Cmd.batch [ requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to divide by zero."), switchHighlightCmd, removeHighlightCmd ]
                                                    )
                                                else
                                                    (
                                                    updatedModel
                                                    , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                    )

                                Indirect regIndex ->
                                    let
                                        accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                        maybePointer = getRegisterValue regIndex model
                                    in
                                    case maybePointer of
                                        Nothing ->
                                            -- parsing erorr
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just pointer ->
                                            let
                                                maybeValue = getRegisterValue pointer model
                                            in
                                            case maybeValue of
                                                Nothing ->
                                                    -- runtime error
                                                    ( 
                                                        { model 
                                                            | instructionPointer = nextInstructionPointer
                                                            , executedInstructions = model.executedInstructions + 1 
                                                            , instructions = updateInstructionAt model.instructionPointer (Div (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex + calculateLogTime model.logBase 0
                                                        }
                                                        , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to access a non-existent register.")
                                                    )
                                                Just value ->
                                                    let
                                                        newLogTime = model.logTime + calculateLogTime model.logBase accVal + calculateLogTime model.logBase value + calculateLogTime model.logBase pointer + calculateLogTime model.logBase regIndex
                                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace 0 (accVal // value) model.logBase model.registers
                                                    in
                                                    if dontHighlight && value /= 0 then
                                                        ( { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Div (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                            , logSpace = newLogSpace
                                                        }
                                                        , Cmd.none
                                                        )
                                                    else if dontHighlight && value == 0 then
                                                        ( { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Div (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                        }
                                                        , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to divide by zero.")
                                                        )
                                                    else 
                                                        let
                                                            updatedModel =
                                                                if value /= 0 then
                                                                    { model
                                                                        | instructionPointer = nextInstructionPointer
                                                                        , registers = newRegisters
                                                                        , executedInstructions = model.executedInstructions + 1
                                                                        , instructions = updateInstructionAt model.instructionPointer (Div (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                        
                                                                        , logTime = newLogTime
                                                                        , logSpace = newLogSpace

                                                                        , highlighted_registers = Dict.insert pointer "bg-blue-200" Dict.empty 
                                                                    }
                                                                else
                                                                    { model 
                                                                        | instructionPointer = nextInstructionPointer
                                                                        , executedInstructions = model.executedInstructions + 1
                                                                        , instructions = updateInstructionAt model.instructionPointer (Div (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                        
                                                                        , logTime = newLogTime
                                                                        
                                                                        , highlighted_registers = Dict.insert pointer "bg-red-200" Dict.empty
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
                                                        if value == 0 then
                                                            (
                                                            updatedModel
                                                            , Cmd.batch [ requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to divide by zero."), switchHighlightCmd, removeHighlightCmd ]
                                                            )
                                                        else
                                                            (
                                                            updatedModel
                                                            , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                            )

                -- JUMP: Set the instruction pointer to the target label.
                Jump idx jumpThere isError exeCount ->
                    case isError of
                        Just _ ->
                            ( 
                                { model 
                                    | instructionPointer = nextInstructionPointer 
                                }
                                , Cmd.none 
                            )
                        _ ->
                            ( 
                                { model 
                                    | instructionPointer = idx
                                    , executedInstructions = model.executedInstructions + 1
                                    , instructions = updateInstructionAt model.instructionPointer (Jump idx jumpThere Nothing (exeCount + 1)) model.instructions
                                    , logTime = model.logTime + 1
                                }
                                , Cmd.none 
                            )

                -- JZERO: Jump if the accumulator is zero.
                Jzero idx jumpThere isError exeCount ->
                    case isError of
                        Just _ ->
                            ( 
                                { model 
                                    | instructionPointer = nextInstructionPointer 
                                }
                                , Cmd.none 
                            )
                        _ ->
                            let
                                accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                newLogTime = model.logTime + calculateLogTime model.logBase accVal
                            in
                            if accVal == 0 then
                                ( 
                                    { model 
                                        | instructionPointer = idx
                                        , executedInstructions = model.executedInstructions + 1 
                                        , instructions = updateInstructionAt model.instructionPointer (Jzero idx jumpThere Nothing (exeCount + 1)) model.instructions
                                        , logTime = newLogTime
                                    }
                                    , Cmd.none 
                                )
                            else
                                ( 
                                    { model 
                                        | instructionPointer = nextInstructionPointer
                                        , executedInstructions = model.executedInstructions + 1 
                                        , instructions = updateInstructionAt model.instructionPointer (Jzero idx jumpThere Nothing (exeCount + 1)) model.instructions
                                        , logTime = newLogTime
                                    }
                                    , Cmd.none 
                                )

                -- JGTZ: Jump if the accumulator is greater than zero.
                Jgtz idx jumpThere isError exeCount ->
                    case isError of
                        Just _ ->
                            ( 
                                { model 
                                    | instructionPointer = nextInstructionPointer 
                                }
                                , Cmd.none 
                            )
                        _ ->
                            let
                                accVal = Maybe.withDefault 0 (getRegisterValue 0 model)
                                newLogTime = model.logTime + calculateLogTime model.logBase accVal
                            in
                            if accVal > 0 then
                                ( 
                                    { model 
                                        | instructionPointer = idx
                                        , executedInstructions = model.executedInstructions + 1 
                                        , instructions = updateInstructionAt model.instructionPointer (Jgtz idx jumpThere Nothing (exeCount + 1)) model.instructions
                                        , logTime = newLogTime
                                    }
                                    , Cmd.none 
                                )
                            else
                                ( 
                                    { model 
                                        | instructionPointer = nextInstructionPointer
                                        , executedInstructions = model.executedInstructions + 1 
                                        , instructions = updateInstructionAt model.instructionPointer (Jgtz idx jumpThere Nothing (exeCount + 1)) model.instructions
                                        , logTime = newLogTime
                                    }
                                    , Cmd.none 
                                )

                -- HALT: End the program.
                Halt exeCount ->
                    ( 
                        { model 
                            | halted = True
                            , isRunning = False
                            , executedInstructions = model.executedInstructions + 1
                            , instructions = updateInstructionAt model.instructionPointer (Halt (exeCount + 1)) model.instructions 
                            , logTime = model.logTime + 1
                        }
                        , Task.perform (ComputeAndPrintDuration True) Time.now
                    )

                -- LABEL: Do nothing (labels are stored in model.labels).
                Label _ _ ->
                    ( { model | instructionPointer = nextInstructionPointer }
                    , Cmd.none 
                    )

                UnknownInstruction ->
                    ( { model | instructionPointer = nextInstructionPointer }
                    , Cmd.none 
                    )

                Read operand isError exeCount ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }
                            , Cmd.none 
                            )
                        _ ->
                            case operand of
                                --parsing error
                                Constant _ ->
                                    ( { model
                                        | instructionPointer = nextInstructionPointer
                                    }
                                    , Cmd.none
                                    )

                                Direct regIndex ->
                                    case Array.get model.inputTapePointer model.inputTape of
                                        Nothing ->
                                            -- runtime error - empty tape
                                            ( { model
                                                | instructionPointer = nextInstructionPointer
                                                , executedInstructions = model.executedInstructions + 1
                                                , instructions = updateInstructionAt model.instructionPointer (Read (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                , logTime = model.logTime + calculateLogTime model.logBase regIndex + calculateLogTime model.logBase 0
                                            }
                                            , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to read from the input tape, while no more values are available.")
                                            )

                                        Just val ->
                                            let
                                                maybePointer = getRegisterValue regIndex model
                                            in
                                            case maybePointer of
                                                Nothing ->
                                                    -- parsing error - register not found
                                                    ( { model | instructionPointer = nextInstructionPointer }
                                                    , Cmd.none
                                                    )
                                                Just _ -> 
                                                    let
                                                        newTapePointer = model.inputTapePointer + 1
                                                        newLogTime = model.logTime + calculateLogTime model.logBase regIndex + calculateLogTime model.logBase val
                                                        ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace regIndex val model.logBase model.registers
                                                    in
                                                    if dontHighlight then
                                                        ( { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , registers = newRegisters
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Read (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logSpace = newLogSpace
                                                            , logTime = newLogTime
                                                            
                                                            , inputTapePointer = newTapePointer
                                                        }
                                                        , Cmd.none
                                                        )
                                                    else
                                                        let 
                                                            updatedModel = { model
                                                                            | instructionPointer = nextInstructionPointer
                                                                            , registers = newRegisters
                                                                            , executedInstructions = model.executedInstructions + 1
                                                                            , instructions = updateInstructionAt model.instructionPointer (Read (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                                            
                                                                            , logSpace = newLogSpace
                                                                            , logTime = newLogTime
                                                                            
                                                                            , inputTapePointer = newTapePointer
                                                                            , highlighted_input_tape = Dict.insert model.inputTapePointer "bg-blue-200" Dict.empty
                                                                        }
                                                            
                                                            switchHighlightCmd =
                                                                Task.perform (\_ -> SwitchHighlight (0, model.inputTapePointer) (1, regIndex, "bg-blue-200"))
                                                                    (Process.sleep (toFloat (highlightDuration // 2)))
                                                            
                                                            removeHighlightCmd =
                                                                Task.perform (\_ -> RemoveHighlightFromRegisters regIndex)
                                                                    (Process.sleep (toFloat highlightDuration))
                    
                                                                
                                                        in
                                                        ( updatedModel
                                                        , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                        )


                                Indirect regIndex ->
                                    case Array.get model.inputTapePointer model.inputTape of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( 
                                                { model 
                                                    | instructionPointer = nextInstructionPointer
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Read (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logTime = model.logTime + calculateLogTime model.logBase regIndex + calculateLogTime model.logBase 0 + calculateLogTime model.logBase ( getRegisterValue regIndex model |> Maybe.withDefault 0 )
                                                }
                                            , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to read from the input tape, while no more values are available.")
                                            )

                                        Just valueToRead ->
                                            let
                                                maybePointer = getRegisterValue regIndex model
                                            in
                                            case maybePointer of
                                                Nothing ->
                                                    -- parsing error - register not found
                                                    ( { model | instructionPointer = nextInstructionPointer }
                                                    , Cmd.none
                                                    )
                                                Just pointer ->
                                                        let
                                                            maybeValue = getRegisterValue pointer model
                                                        in
                                                        case maybeValue of
                                                            Nothing ->
                                                                -- runtime error - reading into non existing register
                                                                ( 
                                                                    { model 
                                                                        | instructionPointer = nextInstructionPointer
                                                                        , executedInstructions = model.executedInstructions + 1
                                                                        , instructions = updateInstructionAt model.instructionPointer (Read (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                        
                                                                        , logTime = model.logTime + calculateLogTime model.logBase regIndex + calculateLogTime model.logBase valueToRead + calculateLogTime model.logBase pointer 
                                                                    }
                                                                , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to access a non-existent register.")
                                                                )
                                                            Just _ ->
                                                                let
                                                                    newTapePointer = model.inputTapePointer + 1
                                                                    newLogTime = model.logTime + calculateLogTime model.logBase regIndex + calculateLogTime model.logBase valueToRead + calculateLogTime model.logBase pointer
                                                                    ( newRegisters, newLogSpace ) = updateRegisterAndComputeLogSpace pointer valueToRead model.logBase model.registers
                                                                in
                                                                if dontHighlight then
                                                                    ( { model
                                                                        | instructionPointer = nextInstructionPointer
                                                                        , registers = newRegisters
                                                                        , executedInstructions = model.executedInstructions + 1
                                                                        , instructions = updateInstructionAt model.instructionPointer (Read (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                        
                                                                        , logTime = newLogTime
                                                                        , logSpace = newLogSpace

                                                                        , inputTapePointer = newTapePointer
                                                                    }
                                                                    , Cmd.none
                                                                    )
                                                                else
                                                                    let 
                                                                        updatedModel = { model
                                                                                            | instructionPointer = nextInstructionPointer
                                                                                            , registers = newRegisters
                                                                                            , executedInstructions = model.executedInstructions + 1
                                                                                            , instructions = updateInstructionAt model.instructionPointer (Read (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                                            
                                                                                            , logSpace = newLogSpace
                                                                                            , logTime = newLogTime
                                                                                            
                                                                                            , inputTapePointer = newTapePointer
                                                                                            , highlighted_input_tape = Dict.insert model.inputTapePointer "bg-blue-200" Dict.empty
                                                                                        }
                                                                        switchHighlightCmd =
                                                                            Task.perform (\_ -> SwitchHighlight (0, model.inputTapePointer) (1, pointer, "bg-blue-200"))
                                                                                (Process.sleep (toFloat (highlightDuration // 2)))
                                                                        
                                                                        removeHighlightCmd =
                                                                            Task.perform (\_ -> RemoveHighlightFromRegisters pointer)
                                                                                (Process.sleep (toFloat highlightDuration))

                                                                    in
                                                                    ( updatedModel
                                                                    , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                                    )

                
                Write operand isError exeCount ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }
                            , Cmd.none 
                            )
                        _ ->
                            case operand of
                                Constant _ ->
                                    -- parsing error : skip for constant
                                    ( { model | instructionPointer = nextInstructionPointer }, Cmd.none)

                                Direct regIndex ->
                                    let
                                        maybeValue = getRegisterValue regIndex model
                                    in
                                    case maybeValue of
                                        Nothing ->
                                            -- parsing error
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just value ->
                                            let
                                                updatedOutputTape = Array.push value model.outputTape
                                                newLogTime = model.logTime + calculateLogTime model.logBase regIndex + calculateLogTime model.logBase value
                                            in
                                            if dontHighlight then
                                                ( { model
                                                    | instructionPointer = nextInstructionPointer
                                                    , executedInstructions = model.executedInstructions + 1
                                                    , instructions = updateInstructionAt model.instructionPointer (Write (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                    
                                                    , logTime = newLogTime
                                                    
                                                    , outputTape = updatedOutputTape
                                                }
                                                , Cmd.none
                                                )
                                            else
                                                let
                                                    updatedModel = { model
                                                                        | instructionPointer = nextInstructionPointer
                                                                        , executedInstructions = model.executedInstructions + 1
                                                                        , instructions = updateInstructionAt model.instructionPointer (Write (Direct regIndex) Nothing (exeCount + 1)) model.instructions
                                                                        
                                                                        , logTime = newLogTime
                                                                        
                                                                        , outputTape = updatedOutputTape
                                                                        , highlighted_registers = Dict.insert regIndex "bg-blue-200" Dict.empty
                                                                    }
                                                    
                                                    lastIdx = List.length (Array.toList (updatedOutputTape)) - 1
                                                    
                                                    switchHighlightCmd =
                                                        Task.perform (\_ -> SwitchHighlight (1, regIndex) (2, lastIdx, "bg-blue-200"))
                                                            (Process.sleep (toFloat (highlightDuration // 2)))
                                                    
                                                    removeHighlightCmd =
                                                        Task.perform (\_ -> RemoveHighlightFromOutputTape lastIdx)
                                                            (Process.sleep (toFloat highlightDuration))

                                                in
                                                ( updatedModel
                                                , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                )

                                Indirect regIndex ->
                                    let
                                        maybePointer = getRegisterValue regIndex model
                                    in
                                    case maybePointer of
                                        Nothing ->
                                            -- ERRORRRRRRRRRRRRRRRRRRR
                                            ( { model | instructionPointer = nextInstructionPointer }
                                            , Cmd.none 
                                            )
                                        Just pointer ->
                                            let
                                                maybeValue = getRegisterValue pointer model
                                            in
                                            case maybeValue of
                                                Nothing ->
                                                    -- runtime error
                                                    ( 
                                                        { model 
                                                            | instructionPointer = nextInstructionPointer
                                                            , instructions = updateInstructionAt model.instructionPointer (Write (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            , executedInstructions = model.executedInstructions + 1
                                                            
                                                            , logTime = model.logTime + calculateLogTime model.logBase regIndex + calculateLogTime model.logBase pointer + calculateLogTime model.logBase 0
                                                        }
                                                        , requestAddMessage (ErrorMessage, "Runtime Error: Instruction " ++ String.fromInt (model.instructionPointer + 1) ++ " attempted to access a non-existent register.")
                                                    )
                                                Just value ->
                                                    let
                                                        updatedOutputTape = Array.push value model.outputTape
                                                        newLogTime = model.logTime + calculateLogTime model.logBase regIndex + calculateLogTime model.logBase pointer + calculateLogTime model.logBase value
                                                    in
                                                    if dontHighlight then
                                                        ( { model
                                                            | instructionPointer = nextInstructionPointer
                                                            , executedInstructions = model.executedInstructions + 1
                                                            , instructions = updateInstructionAt model.instructionPointer (Write (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                            
                                                            , logTime = newLogTime
                                                            
                                                            , outputTape = updatedOutputTape
                                                        }
                                                        , Cmd.none
                                                        )
                                                    else
                                                        let
                                                            updatedModel = { model
                                                                                | instructionPointer = nextInstructionPointer
                                                                                , executedInstructions = model.executedInstructions + 1
                                                                                , instructions = updateInstructionAt model.instructionPointer (Write (Indirect regIndex) Nothing (exeCount + 1)) model.instructions
                                                                                
                                                                                , logTime = newLogTime
                                                                                
                                                                                , outputTape = updatedOutputTape
                                                                                , highlighted_registers = Dict.insert pointer "bg-blue-200" Dict.empty
                                                                            }
                                                            lastIdx = List.length (Array.toList (updatedOutputTape)) - 1
                                                            
                                                            switchHighlightCmd =
                                                                Task.perform (\_ -> SwitchHighlight (1, pointer) (2, lastIdx, "bg-blue-200"))
                                                                    (Process.sleep (toFloat (highlightDuration // 2)))
                                                            
                                                            removeHighlightCmd =
                                                                Task.perform (\_ -> RemoveHighlightFromOutputTape lastIdx)
                                                                    (Process.sleep (toFloat highlightDuration))
                                                        in
                                                        (
                                                        updatedModel
                                                        , Cmd.batch [ switchHighlightCmd, removeHighlightCmd ]
                                                        )
