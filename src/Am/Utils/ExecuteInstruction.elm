module Am.Utils.ExecuteInstruction exposing (..)

import Am.Types.Model exposing (Model)
import Am.Types.Messages exposing (Msg(..))
import Am.Types.Instructions exposing (Instruction(..))

import Dict
import Task
import Process

runAllInstructions : Model -> Model
runAllInstructions model =
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
                dontHighlight = (model.speedIdx == 7 ) || (model.speedIdx == 6 && model.isRunning)
            in
            case instr of
                Increment reg isError ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }, Cmd.none )
                        _ ->
                            if dontHighlight then
                                ( { model 
                                    | registers = (Dict.update reg (Maybe.map (\val -> val + 1)) model.registers)
                                    , instructionPointer = nextInstructionPointer
                                    , executedInstructions = model.executedInstructions + 1    
                                 }
                                , Cmd.none )
                            else
                                let
                                    updatedRegisters =
                                        Dict.update reg (Maybe.map (\val -> val + 1)) model.registers

                                    updatedModel =
                                        { model
                                            | registers = updatedRegisters
                                            , instructionPointer = nextInstructionPointer
                                            , highlighted = Dict.insert reg "bg-green-200" Dict.empty
                                            , executedInstructions = model.executedInstructions + 1   
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
                            if dontHighlight then
                                (
                                { model 
                                    | registers = (Dict.update reg (Maybe.map (\val -> Basics.max 0 (val - 1))) model.registers)
                                    , instructionPointer = nextInstructionPointer
                                    , executedInstructions = model.executedInstructions + 1
                                }
                                , Cmd.none )
                            else
                                let
                                    updatedRegisters =
                                        Dict.update reg (Maybe.map (\val -> Basics.max 0 (val - 1))) model.registers

                                    updatedModel =
                                        { model
                                            | registers = updatedRegisters
                                            , instructionPointer = nextInstructionPointer
                                            , highlighted = Dict.insert reg "bg-yellow-200" Dict.empty
                                            , executedInstructions = model.executedInstructions + 1   
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
                                conditionValue =
                                    Dict.get conditionIndex model.registers
                                        |> Maybe.withDefault 0
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
                                conditionValue =
                                    Dict.get conditionIndex model.registers
                                        |> Maybe.withDefault 0
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