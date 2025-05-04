module Am.Utils.AmInterpreter exposing (..)

import Am.Types.Model exposing (Model)
import Am.Types.Messages exposing (Msg(..))
import Am.Types.Instructions exposing (Instruction(..))

import Dict
import Task
import Process

runAllInstructions : Model -> Model
runAllInstructions model =
    if model.executedInstructions >= model.totalMaxExecutedInstructions then
        model
    else
        if model.instructionPointer >= List.length model.instructions then
            model
        else
            let
                (nextModel, _) = executeInstruction model 0
            in
            runAllInstructions nextModel

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
            ( { model | isRunning = False }, Cmd.none )

        Just instr ->
            let 
                dontHighlight : Bool
                dontHighlight = model.speedIdx == 7 && model.isRunning
            in
            case instr of
                Increment reg isError ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }, Cmd.none )
                        _ ->
                            let
                                newRegisters = Dict.update reg (Maybe.map (\(val, _) -> (val + 1, True))) model.registers
                            in
                            if dontHighlight then
                                ( { model 
                                    | registers = newRegisters
                                    , instructionPointer = nextInstructionPointer
                                    , executedInstructions = model.executedInstructions + 1    
                                 }
                                , Cmd.none )
                            else
                                let
                                    updatedModel =
                                        { model
                                            | registers = newRegisters
                                            , instructionPointer = nextInstructionPointer
                                            , executedInstructions = model.executedInstructions + 1   
                                            
                                            , highlighted = Dict.insert reg "bg-green-200" Dict.empty
                                        }
                                in
                                ( updatedModel
                                , Task.perform (\_ -> RemoveHighlight reg) (Process.sleep (toFloat highlightDuration))
                                )

                Decrement reg isError ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }, Cmd.none )
                        _ ->
                            let
                                newRegisters = Dict.update reg (Maybe.map (\(val, _) -> (Basics.max 0 (val - 1), True))) model.registers
                            in
                            if dontHighlight then
                                (
                                { model 
                                    | registers = newRegisters
                                    , instructionPointer = nextInstructionPointer
                                    , executedInstructions = model.executedInstructions + 1
                                }
                                , Cmd.none )
                            else
                                let
                                    updatedModel =
                                        { model
                                            | registers = newRegisters
                                            , instructionPointer = nextInstructionPointer
                                            , executedInstructions = model.executedInstructions + 1   
                                            
                                            , highlighted = Dict.insert reg "bg-yellow-200" Dict.empty
                                        }
                                in
                                ( updatedModel
                                , Task.perform (\_ -> RemoveHighlight reg) (Process.sleep (toFloat highlightDuration))
                                )

                StartLoop endLoopIndex conditionIndex isError ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }, Cmd.none )
                        _ ->
                            let
                                conditionValue = Dict.get conditionIndex model.registers |> Maybe.map Tuple.first |> Maybe.withDefault 0
                            in
                            if conditionValue == 0 then
                                -- Skip to EndLoop
                                ( { model 
                                    | instructionPointer = endLoopIndex
                                    , executedInstructions = model.executedInstructions + 1
                                }
                                , Cmd.none )
                            else
                                -- Proceed to the next instruction
                                ( { model 
                                    | instructionPointer = nextInstructionPointer 
                                    , executedInstructions = model.executedInstructions + 1    
                                }
                                , Cmd.none )

                EndLoop startLoopIndex conditionIndex isError ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }, Cmd.none )
                        _ ->
                            let
                                conditionValue = Dict.get conditionIndex model.registers |> Maybe.map Tuple.first |> Maybe.withDefault 0
                            in
                            if conditionValue == 0 then
                                -- Go to EndLoop
                                ( { model 
                                    | instructionPointer = nextInstructionPointer
                                    , executedInstructions = model.executedInstructions + 1    
                                }
                                , Cmd.none )
                            else
                                -- Return to the matching StartLoop
                                ( { model
                                    | instructionPointer = startLoopIndex
                                    , executedInstructions = model.executedInstructions + 1   
                                 }
                                 , Cmd.none )

                UnknownInstruction ->
                    -- Ignore unknown instructions and continue
                    ( { model | instructionPointer = nextInstructionPointer }, Cmd.none )