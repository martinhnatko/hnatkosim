module Ram.Utils.Update exposing (..)

import Ram.Types.Messages exposing (Msg)
import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg(..))
import Ram.Utils.RamParser exposing (parseRAM)
import Ram.Utils.ExecuteInstruction exposing (executeInstruction)
import Ram.Utils.HelperFunctions exposing (..)

import Dict
import List exposing (range)
import Array
import Time
import Task

import Shared.Ports exposing (setItem, scrollToBottom)

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCode newCode ->
            let
                (newInstructions1, newLabels) = parseRAM newCode
                newInstructions = Debug.log "newInstructions" newInstructions1
            in
            ( { model | inputText = newCode, instructions = newInstructions, labels = newLabels }, setItem ("ram_current", newCode) )

        Tick _ ->
            if not model.isRunning then
                (model, Cmd.none)
            else
                let
                    defaultSpeed = 1000
                    speed =
                        Array.get (model.speedIdx - 1) model.speeds
                            |> Maybe.withDefault defaultSpeed

                    -- half the speed in ms
                    highlightDuration =
                        speed // 2

                    ( updatedModel, removalCmd ) =
                        executeInstruction model highlightDuration
                in
                ( updatedModel, removalCmd )


        Start ->
            if model.simStarted then
                -- Already started once, so just set isRunning = True
                ( { model | isRunning = True }, Cmd.none )

            else
                -- We have not started before
                let
                    errors =
                        checkForErrors model.instructions  -- e.g. returns List String
                    messages =
                        errors ++ [ "Simulation started" ]
                in
                ( { model
                    | isRunning = True
                    , simStarted = True
                }
                , requestAddMessages messages
                )

        Pause ->
            ( 
                {
                model
                    | isRunning = False
                    , highlighted_input_tape = Dict.empty
                    , highlighted_registers = Dict.empty
                    , highlighted_output_tape = Dict.empty
                }
            , Cmd.none 
            )

        Reset ->
            ( { model
                | isRunning = False
                , simStarted = False
                , instructionPointer = 0
                , registers = Dict.fromList (List.map (\n -> (n,0)) (range 0 100))
                , halted = False
                , inputTapePointer = 0
                , outputTape = Array.empty
                , highlighted_input_tape = Dict.empty
                , highlighted_registers = Dict.empty
                , highlighted_output_tape = Dict.empty
              }
            , requestAddMessages ["Simulation stopped"]
            )
        Step ->
            let
                highlightDuration = 500
                ( newModel, removeHighlightCmd ) =
                    executeInstruction model highlightDuration

                messages =
                    if not newModel.simStarted then
                        let
                            errors = checkForErrors model.instructions
                        in
                        errors ++ [ "Simulation started" ]
                    else
                        []

                -- Now actually set `simStarted = True` if not started yet.
                newModel2 =
                    if not newModel.simStarted then
                        { newModel | simStarted = True }
                    else
                        newModel

                -- Combine highlight removal + any new console messages.
                combinedCmd =
                    Cmd.batch
                        [ removeHighlightCmd
                        , requestAddMessages messages
                        ]
            in
            ( newModel2, combinedCmd )

        
        ChangeSpeed newSpeed ->
            ( { model | speedIdx = newSpeed }, Cmd.none )
        
        RemoveHighlightFromRegisters reg ->
            let
                newHighlighted =
                    Dict.remove reg model.highlighted_registers
            in
            ( { model | highlighted_registers = newHighlighted }, Cmd.none )
        
        RemoveHighlightFromInputTape idx ->
            let
                newHighlighted =
                    Dict.remove idx model.highlighted_input_tape
            in
            ( { model | highlighted_input_tape = newHighlighted }, Cmd.none )

        RemoveHighlightFromOutputTape idx ->
            let
                newHighlighted =
                    Dict.remove idx model.highlighted_output_tape
            in
            ( { model | highlighted_output_tape = newHighlighted }, Cmd.none )
        
        SwitchHighlight (typeSource, source) (typeDest, dest, style) ->
            let 
                newHighlightedInputTape =
                    if typeSource == 0 then
                        Dict.remove source model.highlighted_input_tape
                    else
                        model.highlighted_input_tape

                newHighlightedRegisters =
                    if typeSource == 1 then
                        Dict.remove source model.highlighted_registers
                    else
                        model.highlighted_registers

                newHighlightedOutputTape =
                    if typeSource == 2 then
                        Dict.remove source model.highlighted_output_tape
                    else
                        model.highlighted_output_tape

                finalHighlightedInputTape =
                    if typeDest == 0 then
                        Dict.insert dest style newHighlightedInputTape
                    else
                        newHighlightedInputTape
                
                finalHighlightedRegisters =
                    if typeDest == 1 then
                        Dict.insert dest style newHighlightedRegisters
                    else
                        newHighlightedRegisters
                
                finalHighlightedOutputTape =
                    if typeDest == 2 then
                        Dict.insert dest style newHighlightedOutputTape
                    else
                        newHighlightedOutputTape

            in
            ( { model 
                | highlighted_registers = finalHighlightedRegisters
                , highlighted_input_tape = finalHighlightedInputTape
                , highlighted_output_tape = finalHighlightedOutputTape
              }
              , Cmd.none
            )

        RequestAddMessage newText ->
            ( model
            , Time.now |> Task.perform (\posix -> AddMessageWithTime posix newText)
            )

        AddMessageWithTime posix text ->
            let
                newConsoleMessage =
                    { timestamp = posix
                    , text = text
                    }

                updatedModel =
                    { model
                        | consoleMessages =
                            model.consoleMessages ++ [ newConsoleMessage ]
                    }
            in
            -- After adding the message, scroll to bottom
            ( updatedModel, scrollToBottom "consoleContainer" )
        
        DeleteInput ->
            ( { model
                | isRunning = False
                , inputText = ""
                , simStarted = False
                , instructionPointer = 0
                , registers = Dict.fromList (List.map (\n -> (n,0)) (range 0 100))
                , instructions = []
              }
            , setItem ("ram_current", "")
            )

        SaveSlot i ->
            let
                updatedSlots =
                    Array.set i model.inputText model.slots
                updatedInputTapeSlots = 
                    Array.set i model.inputTape model.slots_input_tapes
            in
            ( { model | slots = updatedSlots 
                      , slots_input_tapes = updatedInputTapeSlots
              }
            ,
            Cmd.batch
                [ setItem ("ram_slot_" ++ String.fromInt i, model.inputText)
                , setItem ("ram_slot_" ++ String.fromInt i ++ "_input_tape", encodeInputTape model.inputTape)
                ]
            )

        DeleteSlot i ->
            let
                updatedSlots =
                    Array.set i "" model.slots
            in
            ( { model | slots = updatedSlots }
            , Cmd.batch
                [ setItem ("ram_slot_" ++ String.fromInt i ++ "_input_tape", "")
                , setItem ("ram_slot_" ++ String.fromInt i, "")
                ] 
            )

        LoadSlot i ->
            let
                maybeCode = Array.get i model.slots
                maybeInputTape = Debug.log "maaybeinpittape" (Array.get i model.slots_input_tapes)
                actualInputTape = Maybe.withDefault Array.empty maybeInputTape
            in
            case maybeCode of
                Nothing ->
                    ( model, Cmd.none ) 
                Just code ->
                    let
                        (instructions, labels) = parseRAM code
                    in
                    ( { model | inputText = code, instructions = instructions, labels = labels, inputTape = actualInputTape }
                    , Cmd.batch
                        [ setItem ("ram_current", code)
                        , setItem ("ram_current_input_tape", encodeInputTape actualInputTape)
                        ]
                    )
        
        ToggleSlotsModal ->
            ( { model | showSlotsModal = not model.showSlotsModal }, Cmd.none )
        
        UpdateInputTape idx value ->
            let
                updatedTape = Array.set idx value model.inputTape
                encodedTape = encodeInputTape updatedTape
            in
            ( { model | inputTape = updatedTape }, setItem ("ram_current_input_tape", encodedTape) )
        
        AddCellToInputTape ->
            let
                updatedTape = Array.push 0 model.inputTape
                encodedTape = encodeInputTape updatedTape
            in
            ( { model | inputTape = updatedTape }, setItem ("ram_current_input_tape", encodedTape))
        
        RemoveLastCell ->
            let
                len = Array.length model.inputTape
            in
            if len > 0 then
                let
                    updatedTape = Array.slice 0 (len - 1) model.inputTape
                    encodedTape = encodeInputTape updatedTape
                in
                ( { model | inputTape = updatedTape }, setItem ("ram_current_input_tape", encodedTape)) 
            else
                ( model, Cmd.none )
        NoOp ->
            ( model, Cmd.none )
            