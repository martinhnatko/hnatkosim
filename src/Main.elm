port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, textarea, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (range)
import String
import MyAbacusParser exposing(Instruction(..))
import Html.Attributes exposing (placeholder)
import MyAbacusParser exposing (parseInstructions, LoopStack)
import Debug
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Html exposing (input)
import Html.Attributes exposing (type_, id)
import Html.Attributes exposing (step)
import Html exposing (ul)
import Html exposing (li)
import Html exposing (span)
import Array exposing (Array)
import Time
import Html.Events exposing (onClick)
import Svg exposing (svg)
import Svg.Attributes exposing (fill)
import Svg.Attributes exposing (stroke)
import Svg.Attributes exposing (strokeWidth)
import Svg.Attributes exposing (viewBox)
import Svg.Attributes exposing (strokeLinecap)
import Svg.Attributes exposing (strokeLinejoin)
import Svg exposing (path)
import Svg.Attributes exposing (d)
import Task
import Process
import Svg exposing (Svg)
import Svg exposing (g)
import Html exposing (h2)

port scrollToBottom : String -> Cmd msg

port setItem : ( String, String ) -> Cmd msg

port getItem : String -> Cmd msg

port gotItem : ( (String, Maybe String) -> msg ) -> Sub msg



-- MODEL

type alias Model =
    { inputText : String
    , registers : Dict Int Int
    , highlighted : Dict Int String
    , instructions : List Instruction
    , isRunning : Bool
    , simStarted : Bool
    , instructionPointer : Int
    , speeds : Array Int
    , speedIdx : Int
    , consoleMessages : List ConsoleMessage
    , slots : Array String
    , showSlotsModal : Bool
    }
    

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


    
type alias ConsoleMessage =
    { timestamp : Time.Posix
    , text : String
    }

-- MESSAGES

type Msg
    = UpdateCode String
    | Start
    | Tick Time.Posix
    | Pause
    | Reset
    | Step
    | ChangeSpeed Int
    | RemoveHighlight Int
    | RequestAddMessage String  -- Ask for a new console message with the current time
    | AddMessageWithTime Time.Posix String  -- Add a new console message with a given time
    | DeleteInput
    | GotItem (String, Maybe String)
    | SaveSlot Int
    | LoadSlot Int
    | ToggleSlotsModal
    | DeleteSlot Int


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

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCode newCode ->
            let
                loopStack : LoopStack
                loopStack = []
                input = String.toList newCode
                newInstructions = parseInstructions [] loopStack 0 input False
                -- newInstructions = Debug.log "newInstructions" newInstructions1
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

requestAddMessages : List String -> Cmd Msg
requestAddMessages msgs =
    msgs
        |> List.map (\msg -> Time.now |> Task.perform (\posix -> AddMessageWithTime posix msg))
        |> Cmd.batch

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
                Increment reg ->
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

                Decrement reg ->
                    let
                        updatedRegisters =
                            Dict.update reg (Maybe.map (\val -> Basics.max 0 (val - 1))) model.registers

                        updatedModel =
                            { model
                                | registers = updatedRegisters
                                , instructionPointer = nextInstructionPointer
                                , highlighted =
                                    Dict.insert reg "bg-red-200" model.highlighted
                            }
                    in
                    ( updatedModel
                    , Task.perform (\_ -> RemoveHighlight reg) (Process.sleep (toFloat highlightDuration))
                    )

                StartLoop endLoopIndex conditionIndex ->
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

                EndLoop startLoopIndex conditionIndex ->
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

checkForErrors : List Instruction -> List String
checkForErrors instructions =
    let
        unknownInstructions = 
            instructions
                |> List.filter (\i -> i == UnknownInstruction)
                |> List.length
    in
    if unknownInstructions > 0 then
        [ "Error: Found " ++ String.fromInt unknownInstructions ++ " unknown instructions" ]
    else
        []

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

-- Transform your slider speeds from ms to s in the labels
sliderLabel : Int -> Html msg
sliderLabel ms =
    let
        toOneDecimal f =
            (toFloat (round (f * 10))) / 10

        inSeconds =
            toOneDecimal ((toFloat ms) / 1000)

        label =
            if ms == 0 then
                "0"
            else
                String.fromFloat inSeconds
    in
    li [ Html.Attributes.class "flex justify-center relative" ]
        [ span [ Html.Attributes.class "absolute" ]
            [ text label ]
        ]


viewSlider : Int -> Model -> Html Msg
viewSlider currentValue model =
    div [ Html.Attributes.class "flex flex-col p-1 w-full" ]
        [ -- Title with Rocket Icon
          div [ Html.Attributes.class "flex items-center gap-2 text-gray-700" ]
            [ heroiconRocket
            , text "Time between instructions (seconds)"
            ]

        , -- Slider Input
          input
            [ type_ "range"
            , Html.Attributes.class "w-full"
            , Html.Attributes.min "1"
            , Html.Attributes.max "7"
            , step "1"
            , value (String.fromInt currentValue)
            , onInput (String.toInt >> Maybe.withDefault 1 >> ChangeSpeed)
            ]
            []

        , -- Labels for the Slider
          ul [ Html.Attributes.class "flex justify-between w-full px-[10px]" ]
            (List.map sliderLabel (Array.toList model.speeds))
        ]
        
viewInstructions : List Instruction -> Int -> Html Msg
viewInstructions instructions pointer =
    div [ Html.Attributes.class "flex flex-wrap gap-3" ]
        (instructions
            |> List.indexedMap (\index instruction ->
                let
                    isActive =
                        index == pointer

                    instructionText =
                        case instruction of
                            Increment n ->
                                "Add " ++ String.fromInt n

                            Decrement n ->
                                "Sub " ++ String.fromInt n

                            StartLoop _ _ ->
                                "("

                            EndLoop _ conditionIndex ->
                                ")" ++ String.fromInt conditionIndex

                            UnknownInstruction ->
                                "Unknown"

                    -- Distinguish instruction types by background/text color.
                    -- We handle each constructor explicitly, so we can give UnknownInstruction a unique color.
                    typeColorClasses =
                        case instruction of
                            Increment _ ->
                                " bg-green-200 text-green-800"

                            Decrement _ ->
                                " bg-red-200 text-red-800"

                            StartLoop _ _ ->
                                " bg-gray-200 text-gray-900"

                            EndLoop _ _ ->
                                " bg-gray-200 text-gray-900"

                            UnknownInstruction ->
                                " bg-yellow-200 text-yellow-800"

                    -- Base classes: same border thickness & style for everyone
                    baseClasses =
                        "p-2 border-4 border-solid rounded font-mono transition-colors"

                    -- If active, show a blue border & bold text; otherwise a transparent border
                    activeClasses =
                        if isActive then
                            " border-blue-500 font-bold"
                        else
                            " border-transparent"
                in
                div
                    [ Html.Attributes.class (baseClasses ++ typeColorClasses ++ activeClasses) ]
                    [ text instructionText ]
            )
        )




viewRegisters : Dict Int Int -> Dict Int String -> List (Html Msg)
viewRegisters registers highlighted =
    registers
        |> Dict.toList
        |> List.map
            (\(regNum, value) ->
                let
                    highlightClass =
                        Dict.get regNum highlighted
                            |> Maybe.withDefault ""
                in
                div
                    [ Html.Attributes.class
                        ("flex items-center gap-3 p-2 border-b last:border-none font-mono " 
                            ++ highlightClass
                        )
                    ]
                    [ div [ Html.Attributes.class "text-gray-500 w-8 text-right" ]
                        [ text (String.fromInt regNum) ]
                    , div [ Html.Attributes.class "h-5 w-px bg-gray-300" ] []
                    , div [ Html.Attributes.class "flex-1 text-left font-medium text-gray-900" ]
                        [ text (String.fromInt value) ]
                    ]
            )


viewSlotsModal : Model -> Html Msg
viewSlotsModal model =
    div
        [ Html.Attributes.class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
        ]
        [ div
            [ Html.Attributes.class "bg-white p-4 rounded shadow-lg relative"
            ]
            [ -- “X” button in the top-right
              button
                [ Html.Attributes.class "absolute top-2 right-2 text-gray-500 hover:text-gray-700"
                , onClick ToggleSlotsModal
                ]
                [ heroiconX ]

              , viewSlots model
            ]
        ]

viewSlots : Model -> Html Msg
viewSlots model =
    div [ Html.Attributes.class "grid grid-cols-5 gap-4 mt-8" ]
        (List.map
            (\i ->
                let
                    label =
                        "Slot " ++ String.fromInt i

                    maybeCode =
                        Array.get i model.slots

                    isEmpty =
                        case maybeCode of
                            Just code ->
                                code == ""

                            Nothing ->
                                True
                in
                -- A small box for each slot
                div [ Html.Attributes.class "border p-3 rounded bg-white shadow-sm w-full" ]
                    [ -- The slot label
                      div [ Html.Attributes.class "font-bold text-gray-700" ]
                        [ text label ]

                      -- The row of 3 buttons
                    , div [ Html.Attributes.class "flex gap-2 mt-2" ]
                        [ -- Save button
                          button
                            [ Html.Attributes.class 
                                ( "bg-blue-500 text-white px-2 py-1 rounded"
                                    ++ if model.inputText == "" then
                                        " opacity-50 cursor-not-allowed"
                                    else
                                        ""
                                )
                            , onClick (SaveSlot i)
                            ]
                            [ text "Save" ]

                          -- Load button
                        , button
                            [ Html.Attributes.class
                                ( "bg-blue-500 text-white px-2 py-1 rounded"
                                    ++ if isEmpty then
                                        " opacity-50 cursor-not-allowed"
                                       else
                                        ""
                                )
                            , onClick (LoadSlot i)
                            , disabled isEmpty
                            ]
                            [ text "Load" ]

                          -- Delete (trash icon)
                        , button
                            [ Html.Attributes.class
                                ( "bg-red-500 text-white px-2 py-1 rounded flex items-center justify-center"
                                    ++ if isEmpty then
                                        " opacity-50 cursor-not-allowed"
                                       else
                                        ""
                                )
                            , onClick (DeleteSlot i)
                            , disabled isEmpty
                            ]
                            [ heroiconTrashSmall ]
                        ]
                    ]
            )
            (List.range 1 20)
        )


--Conole view
viewConsole : List ConsoleMessage -> Html msg
viewConsole consoleMessages =
    div [ Html.Attributes.class "mt-3 bg-gray-800 text-white p-3 rounded shadow-lg" ]
        [ div
            [ id "consoleContainer"
            , Html.Attributes.class "font-mono text-sm h-24 overflow-y-auto"
            ]
            (consoleMessages
                |> List.map (\msg ->
                    div [ Html.Attributes.class "py-1" ]
                        [ text ("[" ++ formatTime msg.timestamp ++ "] " ++ msg.text) ]
                )
            )
        ]

formatTime : Time.Posix -> String
formatTime posix =
    let
        hh = Time.toHour Time.utc posix + 1
        mm = Time.toMinute Time.utc posix
        ss = Time.toSecond Time.utc posix
        twoDigits n = String.padLeft 2 '0' (String.fromInt n)
    in
    twoDigits hh ++ ":" ++ twoDigits mm ++ ":" ++ twoDigits ss


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


heroiconPlay : Html msg
heroiconPlay =
    svg
        [ Svg.Attributes.class "h-6 w-6"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , viewBox "0 0 24 24"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path [ d "M5.25 5.25v13.5l10.5-6.75-10.5-6.75z" ] [] ]


heroiconPause : Html msg
heroiconPause =
    svg
        [ Svg.Attributes.class "h-6 w-6"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , viewBox "0 0 24 24"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path [ d "M15.75 5.25v13.5m-7.5-13.5v13.5" ] [] ]


heroiconStep : Html msg
heroiconStep =
    svg
        [ Svg.Attributes.class "h-6 w-6"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , viewBox "0 0 24 24"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path [ d "m5.25 4.5 7.5 7.5-7.5 7.5m6-15 7.5 7.5-7.5 7.5" ] [] ]


heroiconReset : Html msg
heroiconReset =
    svg
        [ Svg.Attributes.class "h-6 w-6"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , viewBox "0 0 24 24"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path [ d "M5.25 7.5A2.25 2.25 0 0 1 7.5 5.25h9a2.25 2.25 0 0 1 2.25 2.25v9a2.25 2.25 0 0 1-2.25 2.25h-9a2.25 2.25 0 0 1-2.25-2.25v-9Z" ] [] ]

heroiconRocket : Html msg
heroiconRocket =
    svg
        [ Svg.Attributes.class "h-5 w-5"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , viewBox "0 0 24 24"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path
            [ d "M15.59 14.37a6 6 0 0 1-5.84 7.38v-4.8m5.84-2.58a14.98 14.98 0 0 0 6.16-12.12A14.98 14.98 0 0 0 9.631 8.41m5.96 5.96a14.926 14.926 0 0 1-5.841 2.58m-.119-8.54a6 6 0 0 0-7.381 5.84h4.8m2.581-5.84a14.927 14.927 0 0 0-2.58 5.84m2.699 2.7c-.103.021-.207.041-.311.06a15.09 15.09 0 0 1-2.448-2.448 14.9 14.9 0 0 1 .06-.312m-2.24 2.39a4.493 4.493 0 0 0-1.757 4.306 4.493 4.493 0 0 0 4.306-1.758M16.5 9a1.5 1.5 0 1 1-3 0 1.5 1.5 0 0 1 3 0Z" ]
            []
        ]

-- The trash icon
heroiconTrash : Html msg
heroiconTrash =
    svg
        [ Svg.Attributes.class "h-10 w-10"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "currentColor"
        , Svg.Attributes.strokeWidth "1.5"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        ]
        [ path 
            [ Svg.Attributes.d "m14.74 9-.346 9m-4.788 0L9.26 9m9.968-3.21c.342.052.682.107 1.022.166m-1.022-.165L18.16 19.673a2.25 2.25 0 0 1-2.244 2.077H8.084a2.25 2.25 0 0 1-2.244-2.077L4.772 5.79m14.456 0a48.108 48.108 0 0 0-3.478-.397m-12 .562c.34-.059.68-.114 1.022-.165m0 0a48.11 48.11 0 0 1 3.478-.397m7.5 0v-.916c0-1.18-.91-2.164-2.09-2.201a51.964 51.964 0 0 0-3.32 0c-1.18.037-2.09 1.022-2.09 2.201v.916m7.5 0a48.667 48.667 0 0 0-7.5 0" ]
            []
        ]


-- The trash icon small
heroiconTrashSmall : Html msg
heroiconTrashSmall =
    svg
        [ Svg.Attributes.class "h-6 w-6"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "currentColor"
        , Svg.Attributes.strokeWidth "1.5"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        ]
        [ path 
            [ Svg.Attributes.d "m14.74 9-.346 9m-4.788 0L9.26 9m9.968-3.21c.342.052.682.107 1.022.166m-1.022-.165L18.16 19.673a2.25 2.25 0 0 1-2.244 2.077H8.084a2.25 2.25 0 0 1-2.244-2.077L4.772 5.79m14.456 0a48.108 48.108 0 0 0-3.478-.397m-12 .562c.34-.059.68-.114 1.022-.165m0 0a48.11 48.11 0 0 1 3.478-.397m7.5 0v-.916c0-1.18-.91-2.164-2.09-2.201a51.964 51.964 0 0 0-3.32 0c-1.18.037-2.09 1.022-2.09 2.201v.916m7.5 0a48.667 48.667 0 0 0-7.5 0" ]
            []
        ]

-- The x icon
heroiconX : Html msg
heroiconX =
    svg
        [ Svg.Attributes.class "h-10 w-10"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "currentColor"
        , Svg.Attributes.strokeWidth "1.5"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        ]
        [ path 
            [ Svg.Attributes.d "M6 18 18 6M6 6l12 12" ]
            []
        ]


-- The save
heroiconSave : Html msg
heroiconSave =
    svg
        [ Svg.Attributes.class "h-6 w-6"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "currentColor"
        , Svg.Attributes.strokeWidth "1.5"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        ]
        [ path 
            [ Svg.Attributes.d "M11 16h2m6.707-9.293-2.414-2.414A1 1 0 0 0 16.586 4H5a1 1 0 0 0-1 1v14a1 1 0 0 0 1 1h14a1 1 0 0 0 1-1V7.414a1 1 0 0 0-.293-.707ZM16 20v-6a1 1 0 0 0-1-1H9a1 1 0 0 0-1 1v6h8ZM9 4h6v3a1 1 0 0 1-1 1h-4a1 1 0 0 1-1-1V4Z" ]
            []
        ]
-- <svg class="w-6 h-6 text-gray-800 dark:text-white" aria-hidden="true" xmlns="http://www.w3.org/2000/svg" width="24" height="24" fill="none" viewBox="0 0 24 24">
--   <path stroke="currentColor" stroke-linecap="round" stroke-width="2" d=""/>
-- </svg>
