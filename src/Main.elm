port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, textarea, text, input)
import Html.Attributes exposing (value, disabled)
import Html.Events exposing (onInput, onClick)
import Dict
import List exposing (range)
import String
import Time
import Task
import Array

import Utils.AbacusParser exposing(parseInstructions)
import Views.Console exposing (viewConsole)
import Types.Messages exposing (Msg(..))
import Types.Model exposing (Model)
import Types.Instructions exposing (Instruction(..))
import Views.SaveSlots exposing (viewSlotsModal)
import Views.Registers exposing (viewRegisters)
import Views.Instructions exposing (viewInstructions)
import Views.SpeedSlider exposing (viewSlider)

import Icons.Pause exposing (heroiconPause)
import Icons.Play exposing (heroiconPlay)
import Icons.Step exposing (heroiconStep)
import Icons.Reset exposing (heroiconReset)
import Icons.Trash exposing (heroiconTrash)
import Icons.Save exposing (heroiconSave)

import Utils.ExecuteInstruction exposing (executeInstruction)
import Utils.HelperFunctions exposing (requestAddMessages, checkForErrors)


-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


-- PORTS
port scrollToBottom : String -> Cmd msg
port setItem : ( String, String ) -> Cmd msg
port getItem : String -> Cmd msg
port gotItem : ( (String, Maybe String) -> msg ) -> Sub msg


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.isRunning then
            let
                defaultSpeed = 1000
                speed =
                    Array.get (model.speedIdx - 1) model.speeds
                        |> Maybe.withDefault defaultSpeed
            in
            Time.every (toFloat speed) Tick
          else
            Sub.none

        , gotItem GotItem
        ]


-- INIT
init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialModel =
            { inputText = ""
            , registers = Dict.fromList (List.map (\n -> (n, 0)) (range 0 100))
            , highlighted = Dict.empty
            , instructions = []
            , isRunning = False
            , simStarted = False
            , instructionPointer = 0
            , speeds = Array.fromList [ 4000 , 2000, 1000, 500, 250, 100, 0 ]
            , speedIdx = 4
            , consoleMessages = []
            , slots = Array.repeat 21 ""
            , showSlotsModal = False
            }
        cmdToLoad =
            Cmd.batch
                [ getItem "current" -- ask for the current code
                , Cmd.batch <|
                    List.map
                        (\i -> getItem ("slot_" ++ String.fromInt i))
                        (List.range 1 20)
                ]
    in
    ( initialModel, cmdToLoad )


-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCode newCode ->
            let
                input = String.toList newCode
                newInstructions = parseInstructions [] [] 0 input False
            in
            ( { model | inputText = newCode, instructions = newInstructions }, setItem ("current", newCode) )

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
            ( { model | isRunning = False }, Cmd.none )

        Reset ->
            ( { model
                | isRunning = False
                , simStarted = False
                , instructionPointer = 0
                , registers = Dict.fromList (List.map (\n -> (n,0)) (range 0 100))
              }
            , requestAddMessages ["Simulation stopped"]
            )
        Step ->
            let
                highlightDuration = 200
                ( newModel1, removeHighlightCmd ) =
                    executeInstruction model highlightDuration

                -- If we haven't started before, create "Simulation started" messages.
                -- Otherwise, no new messages.
                messages =
                    if not newModel1.simStarted then
                        let
                            errors = checkForErrors model.instructions
                        in
                        errors ++ [ "Simulation started" ]
                    else
                        []

                -- Now actually set `simStarted = True` if not started yet.
                newModel2 =
                    if not newModel1.simStarted then
                        { newModel1 | simStarted = True }
                    else
                        newModel1

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
        
        RemoveHighlight reg ->
            let
                newHighlighted =
                    Dict.remove reg model.highlighted
            in
            ( { model | highlighted = newHighlighted }, Cmd.none )

        RequestAddMessage newText ->
            -- We want to add a message with a fresh timestamp
            -- -> ask Elm for current time, then AddMessageWithTime
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
            , setItem ("current", "")
            )
        
        GotItem (key, code) ->
            case key of
                "current" ->
                    -- If localStorage had something for "current", put it in inputText
                    let
                        instructions = parseInstructions [] [] 0 (String.toList (Maybe.withDefault "" code)) False
                    in
                    ( { model | inputText = Maybe.withDefault "" code, instructions = instructions }, requestAddMessages ["Welcome to Abacus Machine Simulator"] )

                _ ->
                    -- Maybe it's one of the 10 slots, like "slot_3"
                    let
                        maybeSlotIndex =
                            if String.startsWith "slot_" key then
                                String.dropLeft 5 key |> String.toInt
                            else
                                Nothing
                    in
                    case maybeSlotIndex of
                        Just i ->
                            let
                                updatedSlots = Array.set i (Maybe.withDefault "" code) model.slots
                            in
                            ( { model | slots = updatedSlots }, Cmd.none )

                        Nothing ->
                            -- Unrecognized key
                            ( model, Cmd.none )

        SaveSlot i ->
            let
                -- Update the local model, so the slot is not empty.
                updatedSlots =
                    Array.set i model.inputText model.slots
            in
            ( { model | slots = updatedSlots }
            , setItem ("slot_" ++ String.fromInt i, model.inputText)
            )

        DeleteSlot i ->
            let
                updatedSlots =
                    Array.set i "" model.slots
            in
            ( { model | slots = updatedSlots }
            , setItem ("slot_" ++ String.fromInt i, "") 
            )

        LoadSlot i ->
            let
                maybeCode = Array.get i model.slots
            in
            case maybeCode of
                Nothing ->
                    ( model, Cmd.none ) 
                Just code ->
                    ( { model | inputText = code, instructions = parseInstructions [] [] 0 (String.toList code) False }
                    , setItem ("current", code) 
                    )
        
        ToggleSlotsModal ->
            ( { model | showSlotsModal = not model.showSlotsModal }, Cmd.none )


-- VIEW
view : Model -> Html Msg
view model =
    let
        atEndOfInstructions : Bool
        atEndOfInstructions =
            model.instructionPointer >= List.length model.instructions
    in
    div [ Html.Attributes.class "flex flex-col h-screen p-2 bg-gray-200" ]
        [ -- Navbar Section
          div [ Html.Attributes.class "flex gap-4 mb-5" ]
            [
            div [ Html.Attributes.class "flex gap-4 w-1/3 pr-3" ]
                [
                    -- If isRunning, show Pause button
                    if model.isRunning then
                        button 
                            [ Html.Attributes.class "w-1/3 px-4 py-2 bg-blue-500 text-white flex items-center justify-center rounded"
                            , onClick Pause 
                            ]
                            [ heroiconPause, text "Pause"  ]
                    else
                        -- Otherwise, show Start button
                        let
                            isDisabled = atEndOfInstructions
                        in
                        button
                            (  [ Html.Attributes.class "w-1/3 px-4 py-2 bg-blue-500 text-white flex items-center justify-center rounded"
                            , onClick Start
                            ]
                            ++ ( if isDisabled then 
                                    [ disabled True
                                    , Html.Attributes.class "bg-gray-400 cursor-not-allowed"
                                    ] 
                                else 
                                    []
                            )
                            )
                            [ heroiconPlay, text "Start" ]

                , -- Step button
                let
                    isDisabled = atEndOfInstructions || model.isRunning
                in
                button
                    (  [ Html.Attributes.class "w-1/3 px-4 py-2 bg-blue-500 text-white flex items-center justify-center rounded"
                        , onClick Step
                        ]
                    ++ ( if isDisabled then 
                            [ disabled True
                            , Html.Attributes.class "bg-gray-400 cursor-not-allowed"
                            ]
                        else
                            []
                        )
                    )
                    [ heroiconStep, text "Step" ]

                , -- Reset button
                if not model.simStarted then
                    button 
                        [ Html.Attributes.class "w-1/3 px-4 py-2 bg-gray-400 text-white flex cursor-not-allowed items-center justify-center rounded"
                        , disabled True
                        ]
                        [ heroiconReset, text "Stop" ]
                else
                    button 
                        [ Html.Attributes.class "w-1/3 px-4 py-2 bg-red-500 text-white flex items-center justify-center rounded"
                        , onClick Reset
                        ]
                        [ heroiconReset, text "Stop" ]
                ]


            , -- Speed Section
            div [ Html.Attributes.class "flex gap-4 w-1/3 pr-8" ]
                [
                viewSlider model.speedIdx model
                ]

            -- Slots Modal Button + possibly the modal itself   
            , 
            div [ Html.Attributes.class "flex gap-4 w-1/3"]
                [ -- Button to open/close the modal
                button
                    [ Html.Attributes.class "border border-blue-500 text-blue-500 bg-white w-1/3 px-1 py-2 flex items-center justify-center rounded"
                    , onClick ToggleSlotsModal
                    ]
                    [ heroiconSave, text "Save/Load" ]

                
                ]
            ]
        , -- Main Content Section
          div [ Html.Attributes.class "flex flex-grow gap-4 overflow-hidden" ]
            [ -- The textarea + trash bin button
            div [ Html.Attributes.class "flex flex-col w-1/3 bg-white p-3 shadow-lg rounded relative" ]
                [ textarea
                    ( [ Html.Attributes.class
                            ( "flex-grow w-full h-full p-2 border rounded resize-none overflow-auto text-lg font-mono "
                                ++ if model.simStarted then
                                    "bg-gray-200 text-gray-500 cursor-not-allowed"
                                else
                                    "bg-white text-black"
                            )
                    , Html.Attributes.placeholder "Enter your code here..."
                    , onInput UpdateCode
                    , value model.inputText
                    ]
                    ++ (if model.simStarted then [ disabled True ] else [])
                    )
                    []
                , -- The trash bin button, only clickable if NOT simStarted, else we might disable or hide
                if not model.simStarted then
                    button 
                        [ Html.Attributes.class "absolute bottom-9 right-10 text-gray-500 hover:text-red-500"
                        , onClick DeleteInput
                        ]
                        [ heroiconTrash ]
                else
                    text ""
                ]

            , -- Instructions Column
            div 
                [ Html.Attributes.class
                    ( "flex flex-col w-1/3 p-3 shadow-lg rounded overflow-auto border-2 border-transparent "
                        ++ if atEndOfInstructions && model.simStarted then
                            " bg-green-50 border-green-400"
                        else
                            " bg-white"
                    )
                ]
                [ viewInstructions model.instructions model.instructionPointer ]

            , -- Registers Column
              div [ Html.Attributes.class "flex flex-col w-1/3 bg-white p-3 shadow-lg rounded overflow-auto" ]
                [ div [] (viewRegisters model.registers model.highlighted) ]
            ]
            -- CONSOLE
            , viewConsole model.consoleMessages

            , if model.showSlotsModal then
                    viewSlotsModal model
                else
                    text ""
        ]