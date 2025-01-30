port module Main exposing (main)

import Browser
import Html exposing (Html, div, text, input)
import Html.Attributes exposing (class)
import Dict
import List exposing (range)
import String
import Time
import Task
import Array

import Types.Messages exposing (Msg(..))
import Types.Model exposing (Model)
import Types.Instructions exposing (Instruction(..))

import Views.HeaderView exposing (headerView)
import Views.MainView exposing (mainContentView)
import Views.FooterView exposing (footerView)
import Views.Header.Menu exposing (viewSlotsModal)

import Utils.ExecuteInstruction exposing (executeInstruction)
import Utils.HelperFunctions exposing (requestAddMessages, checkForErrors)
import Utils.AbacusParser exposing(parseInstructions)


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
    div [ class "flex flex-col h-screen p-2 bg-gray-200" ]
        [ -- Header Section
          headerView model

          -- Main Content Section
        , mainContentView model

          -- Footer Section
        , footerView model

          -- Slots Modal
        , if model.showSlotsModal then
              viewSlotsModal model
          else
              text ""
        ]