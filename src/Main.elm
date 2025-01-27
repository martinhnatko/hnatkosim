module Main exposing (..)

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
import Html.Attributes exposing (class)
import Html exposing (input)
import Html.Attributes exposing (type_)
import Html.Attributes exposing (step)
import Html exposing (ul)
import Html exposing (li)
import Html exposing (span)
import Array exposing (Array)
import Time
import Html.Attributes exposing (start)

import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Svg exposing (svg)
import Svg.Attributes exposing (fill, class)
import Svg.Attributes exposing (stroke)
import Svg.Attributes exposing (strokeWidth, fillRule, clipRule)
import Svg.Attributes exposing (viewBox)
import Svg.Attributes exposing (strokeLinecap)
import Svg.Attributes exposing (strokeLinejoin)
import Svg exposing (path)
import Svg.Attributes exposing (d)
import Task
import Process

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
    }


init : Model
init =
    { inputText = ""
    , registers = Dict.fromList (List.map (\n -> (n, 0)) (range 0 100))
    , highlighted = Dict.empty
    , instructions = []
    , isRunning = False
    , simStarted = False
    , instructionPointer = 0
    , speeds = Array.fromList [ 4000 , 2000, 1000, 500, 250, 100, 0 ]
    , speedIdx = 4
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


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isRunning then
        let
            defaultSpeed = 1000
            speed = Array.get (model.speedIdx - 1) model.speeds
                        |> Maybe.withDefault defaultSpeed
        in
        Time.every (toFloat speed ) Tick
    else
        Sub.none

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =    
    case msg of
        UpdateCode newCode ->
            let
                loopStack : LoopStack
                loopStack = []
                input = String.toList newCode
                newInstructions1 = parseInstructions [] loopStack 0 input
                newInstructions = Debug.log "newInstructions" newInstructions1
            in
            ( { model | inputText = newCode, instructions = newInstructions }, Cmd.none )

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
            ( { model | isRunning = True, simStarted = True }
            , Cmd.none
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
            , Cmd.none
            )
        Step ->
            let
                highlightDuration = 200
                ( updatedModel, removeHighlightCmd ) =
                    executeInstruction model highlightDuration
            in
            ( { updatedModel | simStarted = True }, removeHighlightCmd )

        
        ChangeSpeed newSpeed ->
            ( { model | speedIdx = newSpeed }, Cmd.none )
        
        RemoveHighlight reg ->
            let
                newHighlighted =
                    Dict.remove reg model.highlighted
            in
            ( { model | highlighted = newHighlighted }, Cmd.none )

executeInstruction : Model -> Int -> (Model, Cmd Msg)
executeInstruction model highlightDuration =
    let
        currentInstruction =
            List.head (List.drop model.instructionPointer model.instructions)

        nextInstructionPointer =
            model.instructionPointer + 1

        -- defaultSpeed = 1000
        -- speed =
        --     Array.get (model.speedIdx - 1) model.speeds
        --         |> Maybe.withDefault defaultSpeed

        -- -- half the speed in ms
        -- highlightDuration =
        --     speed // 2
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



view : Model -> Html Msg
view model =
    let
        atEndOfInstructions : Bool
        atEndOfInstructions =
            model.instructionPointer >= List.length model.instructions
    in
    div [ Html.Attributes.class "flex flex-col h-screen p-3 bg-gray-200" ]
        [ -- Navbar Section
          div [ Html.Attributes.class "flex gap-4 mb-4" ]
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
                        [ heroiconReset, text "Reset" ]
                else
                    button 
                        [ Html.Attributes.class "w-1/3 px-4 py-2 bg-red-500 text-white flex items-center justify-center rounded"
                        , onClick Reset
                        ]
                        [ heroiconReset, text "Reset" ]
                ]


            , -- Speed Section
            div [ Html.Attributes.class "flex gap-4 w-1/3 pr-8" ]
                [
                viewSlider model.speedIdx model
                ]
            ]
        , -- Main Content Section
          div [ Html.Attributes.class "flex flex-grow gap-4 overflow-hidden" ]
            [ -- Textarea Column
              div [ Html.Attributes.class "flex flex-col w-1/3 bg-white p-4 shadow-lg rounded" ]
                [ textarea
                    ( [ Html.Attributes.class
                            ( "flex-grow w-full h-full p-2 border rounded resize-none overflow-auto text-xl font-mono "
                                ++ if model.simStarted then
                                    "bg-gray-200 text-gray-500 cursor-not-allowed"
                                else
                                    "bg-white text-black"
                            )
                    , placeholder "Enter your code here..."
                    , onInput UpdateCode
                    , value model.inputText
                    ]
                    ++ (if model.simStarted then [ disabled True ] else [])
                    )
                    []
                ]

            , -- Instructions Column
              div [ Html.Attributes.class "flex flex-col w-1/3 bg-white p-4 shadow-lg rounded overflow-auto" ]
                [ viewInstructions model.instructions model.instructionPointer ]
            , -- Registers Column
              div [ Html.Attributes.class "flex flex-col w-1/3 bg-white p-4 shadow-lg rounded overflow-auto" ]
                [ div [] (viewRegisters model.registers model.highlighted) ]
            ]
        ]

-- View for the slider
viewSlider : Int -> Model -> Html Msg
viewSlider currentValue model =
    div [ Html.Attributes.class "flex flex-col space-y-2 p-2 w-full" ]
        [ -- Slider Input
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

-- Helper function to generate slider labels
sliderLabel : Int -> Html msg
sliderLabel n =
    li [ Html.Attributes.class "flex justify-center relative" ]
        [ span [ Html.Attributes.class "absolute" ] [ Html.text (String.fromInt n) ] ]

        
viewInstructions : List Instruction -> Int -> Html Msg
viewInstructions instructions pointer =
    div [ Html.Attributes.class "flex flex-wrap gap-4" ]
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
                                "Unknown Instruction"

                    -- Base classes: same border thickness & style for everyone
                    baseClasses =
                        "p-2 border-4 border-solid rounded font-mono transition-colors"

                    -- Distinguish instruction types by background/text color
                    typeColorClasses =
                        case instruction of
                            Increment _ ->
                                " bg-green-200 text-green-800"
                            Decrement _ ->
                                " bg-red-200 text-red-800"
                            _ ->
                                " bg-gray-200 text-gray-900"

                    -- If active, show a blue border & bold text; else a transparent border
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
                    [ Html.Attributes.class ("flex items-center gap-3 p-2 border-b last:border-none " ++ highlightClass) ]
                    [ div [ Html.Attributes.class "text-gray-500 w-8 text-right" ]
                        [ text (String.fromInt regNum) ]
                    , div [ Html.Attributes.class "h-5 w-px bg-gray-300" ] []
                    , div [ Html.Attributes.class "flex-1 text-left font-medium text-gray-900" ]
                        [ text (String.fromInt value) ]
                    ]
            )



-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
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
