module Am.Utils.ExecuteInstruction exposing (..)

import Am.Types.Model exposing (Model)
import Am.Types.Messages exposing (Msg(..))
import Am.Types.Instructions exposing (Instruction(..))

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
            ( { model | isRunning = False }, Cmd.none )

        Just instr ->
            case instr of
                Increment reg isError ->
                    case isError of
                        Just _ ->
                            ( { model | instructionPointer = nextInstructionPointer }, Cmd.none )
                        _ ->
                            let
                                updatedRegisters =
                                    Dict.update reg (Maybe.map (\val -> val + 1)) model.registers

                                updatedModel =
                                    { model
                                        | registers = updatedRegisters
                                        , instructionPointer = nextInstructionPointer
                                        , highlighted =
                                            Dict.insert reg "bg-green-200" model.highlighted
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
                                updatedRegisters =
                                    Dict.update reg (Maybe.map (\val -> Basics.max 0 (val - 1))) model.registers

                                updatedModel =
                                    { model
                                        | registers = updatedRegisters
                                        , instructionPointer = nextInstructionPointer
                                        , highlighted =
                                            Dict.insert reg "bg-yellow-200" model.highlighted
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
                                -- Skip to instruction after EndLoop
                                ( { model | instructionPointer = endLoopIndex }, Cmd.none )
                            else
                                -- Proceed to the next instruction
                                ( { model | instructionPointer = nextInstructionPointer }, Cmd.none )

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
                                -- Go next instruction after EndLoop
                                ( { model | instructionPointer = nextInstructionPointer }, Cmd.none )
                            else
                                -- Return to the matching StartLoop
                                ( { model | instructionPointer = startLoopIndex }, Cmd.none )

                UnknownInstruction ->
                    -- Ignore unknown instructions and continue
                    ( { model | instructionPointer = nextInstructionPointer }, Cmd.none )