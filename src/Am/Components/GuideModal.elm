module Am.Components.GuideModal exposing (..)

import Html exposing (Html, div, button, text, h2, p, span, table, thead, tr, th, tbody, td, h3)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, stopPropagationOn)

import Json.Decode as Decode

import Shared.Icons.X exposing (heroiconX)
import Shared.Icons.Guide exposing (heroiconGuide)
import Html exposing (i)

stopPropagationClick : msg -> Html.Attribute msg
stopPropagationClick noOpMsg =
    stopPropagationOn "click" <|
        Decode.succeed ( noOpMsg, True )


viewGuideModal : msg -> (Int -> msg) -> msg -> Html msg
viewGuideModal onToggleGuideModal onLoadSlot onNoOp =
    div
        [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
        , onClick onToggleGuideModal
        ]
        [ div
            [ class "bg-white p-4 rounded shadow-lg relative max-h-[80vh] max-w-[80vw] lg:max-w-[60vw] overflow-y-auto"
            , stopPropagationClick onNoOp
            ]
            [ -- Close Button (top-right)
            button
                [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700 focus:outline-none"
                , onClick onToggleGuideModal
                ]
                [ heroiconX ]

            , h2 [ class "text-xl font-bold mb-2 flex items-center gap-1" ]
                    [ heroiconGuide, text "Abacus Machine Guide" ]
            
            , div [ class "my-1 text-sm" ]
                [ p [ class "mb-2" ]
                    [ 
                        text "Abacus Machine is an abstract model of a simple computing device that operates with an unlimited number of registers, where each register can hold arbitrarily large natural numbers." 
                    ]
                , p [ class "mb-2" ]
                    [ 
                        text "This simulator implements a computational model with all supported instructions and limited number of registers. It offers two modes: a "
                        , span [ class "text-green-500 font-semibold"] [ text"simulation mode" ] 
                        , span [] [ text " and an " ] 
                        , span [ class "text-red-500 font-semibold" ] [ text"editing mode" ]
                        , text ". Once you press "
                        , span [ class "text-blue-500 font-semibold" ] [ text "Step"]
                        , text " or "
                        , span [ class "text-blue-500 font-semibold" ] [ text "Start"]
                        , text ", the simulator switches to " 
                        , span [ class "text-green-500 font-semibold" ] [ text "simulation mode"]
                        , text ", and no further edits are allowed until you press " 
                        , span [ class "text-red-500 font-semibold" ] [ text "Stop"]
                        , text ". You can write comments in the code by using the " 
                        , span [ class "font-bold" ] [ text "'#'"]
                        , text " character." 
                    ]
                , p [ ]
                    [ 
                        text "The simulation can be run step by step or continuously. When using instant speed, there is a risk of getting stuck in an infinite loop, so there is a configurable limit called"
                        , i [ class "font-semibold" ] [ text " 'Maximum number of executed instructions with instant speed'" ]
                        , text ". If your program is complex and needs more instructions, you can increase this limit in the settings." 
                    ]
                ]
            
            -- Example Codes Section
            , h3 [ class "text-lg font-semibold mt-5" ]
                [ text "Example codes:" ]

            , div [ class "flex flex-row gap-3 my-2" ]
                [ 
                -- Example 1
                div [ class "border p-2 rounded bg-white shadow-sm w-full" ]
                    [ div [ class "font-semibold text-gray-700" ]
                        [ text "Example 1: " 
                        , span [ class "font-normal" ] [ text "a+b" ]
                        ]
                    , div [ class "flex flex-row-reverse gap-2 mt-2" ]
                        [ button
                            [ class "bg-blue-500 text-white px-2 py-1 rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                            , onClick (onLoadSlot 21)
                            ]
                            [ text "Load" ]
                        ]
                    ]

                -- Example 2
                , div [ class "border p-2 rounded bg-white shadow-sm w-full" ]
                    [ div [ class "font-semibold text-gray-700" ]
                        [ text "Example 2: " 
                        , span [ class "font-normal" ] [ text "a*b" ]
                        ]
                    , div [ class "flex flex-row-reverse  gap-2 mt-2" ]
                            [ button
                                [ class "bg-blue-500 text-white px-2 py-1 rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                                , onClick (onLoadSlot 22)
                                ]
                                [ text "Load" ]
                            ]
                    ]
                
                -- Example 3
                , div [ class "border p-2 rounded bg-white shadow-sm w-full" ]
                    [ div [ class "font-semibold text-gray-700" ]
                        [ text "Example 3: " 
                        , span [ class "font-normal" ] [ text "n!" ]
                        ]
                    , div [ class "flex flex-row-reverse gap-2 mt-2" ]
                        [ button
                            [ class "bg-blue-500 text-white px-2 py-1 rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                            , onClick (onLoadSlot 23)
                            ]
                            [ text "Load" ]
                        ]
                    ]
                ]


            -- Instructions Section
            , h3 [ class "text-lg font-semibold mt-5" ]
                [ text "Instructions:" ]

            , p [ class "mb-2 text-sm" ]
                [ 
                    text "Everything the parser recognizes is counted as an instruction (parsing errors are not included), including the closing bracket that ends a loop. If the instruction pointer lands on an end loop closing bracket, whose condition register is zero, it does not jump back to the start loop instruction of that loop but continues to the next one." 
                ]

            , table [ class "min-w-full table-fixed my-2 text-sm" ]
                [ thead []
                    [ tr []
                        [ th [ class "w-1/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                            [ text "Instruction" ]
                        , th [ class "w-1/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                            [ text "Example" ]
                        , th [ class "w-4/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                            [ text "Description" ]
                        ]
                    ]

                , tbody [ class "bg-white divide-y divide-gray-300" ]
                    [ 
                    -- Increment operation row
                    tr [ class "bg-green-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-green-500 font-semibold" ]
                                [ text "Increment" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "a1" ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "Adds 1 to the specified register. For example, a1 increments register R1 by 1, changing its value from x to x+1." ]
                        ]

                    -- Decrement operation row
                    , tr [ class "bg-yellow-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-yellow-500 font-semibold" ]
                                [ text "Decrement" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "s3" ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "Subtracts 1 from the specified register if it’s non-zero. For instance, s3 decrements R3 by 1, changing its value from x to x−1. If R3 is already 0, it remains unchanged." ]
                        ]

                    -- startloop
                    , tr [ class "bg-gray-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-gray-500 font-semibold" ]
                                [ text "Start loop" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "(" ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "Marks the beginning of a loop. Execution enters the loop body if the loop’s condition register is greater than zero. If it’s zero, the simulator jumps directly to the loop’s end." ]
                        ]

                    -- endloop
                    , tr [ class "bg-gray-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-gray-500 font-semibold" ]
                                [ text "End loop" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text ")8" ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "Marks the end of the loop. If the loop’s condition register is still greater than zero, execution jumps back to the loop’s start. Otherwise, the simulator continues with the next instruction after the loop." ]
                        ]
                    ]
                ]
              
            
            -- types of errors
            , h3 [ class "text-lg font-semibold mt-5" ]
                [ text "Possible errors:" ]
            
            , p [ class "mb-2 text-sm" ]
                [ 
                    text "This simulator includes four different errors and one warning. All errors are parsing errors, meaning they are detected during parsing which is performed on each input. If you start the simulation with any errors (shown in red), those errors are printed to the console." 
                ]
            , table [ class "min-w-full table-fixed mt-2 text-sm" ]
                [ thead []
                    [ tr []
                        [ 
                        
                        th [ class "w-1/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                            [ text "Type" ]
                        
                        , th [ class "w-1/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                            [ text "Error message" ]
                        
                        , th [ class "w-4/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                            [ text "Cause & Prevention" ]
                        ]
                    ]
                
                , tbody [ class "bg-white divide-y divide-gray-300" ]
                    [     
                    
                    tr [ class "bg-red-50" ]
                        
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-red-500 font-semibold" ]
                                [ text "Parsing error" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "Found unknown instruction." ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "This error occurs when the parser encounters an unrecognized instruction. To prevent it, verify that all instructions match the valid instruction set and check your code for typos." ]
                        ]
                    
                    , tr [ class "bg-red-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-red-500 font-semibold" ]
                                [ text "Parsing error" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "Found instruction that is attempting to access a non-existing register" ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "This error occurs when the parser encounters instruction that references a register index that doesn’t exist or is out of range. To prevent it, ensure all register references are within the valid range." ]
                        ]
                    
                    , tr [ class "bg-red-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-red-500 font-semibold" ]
                                [ text "Parsing error" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "Found unmatched start of the loop" ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "This error occurs when a loop is started without a corresponding end. To prevent it, make sure every loop instruction has a matching closing bracket." ]
                        ]
                    
                    , tr [ class "bg-red-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-red-500 font-semibold" ]
                                [ text "Parsing error" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "Found unmatched end of the loop." ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "This error occurs when a loop is ended without a corresponding start. To prevent it, ensure that each loop end is paired with a start bracket." ]
                        ]
                    ]

                    , tr [ class "bg-yellow-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-yellow-500 font-semibold" ]
                                [ text "Warning" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "Maximum number of instant-speed instructions exceeded." ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "This warning appears when your program exceeds the configured limit of instructions while running at instant speed. It helps prevent the simulator from getting stuck in infinite loops. If your program is complex and needs more instructions, you can raise this limit in the settings." ]
                        ]
                    ]
            ]
        ]
