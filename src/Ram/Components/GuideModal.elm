module Ram.Components.GuideModal exposing (..)

import Html exposing (Html, div, button, text, h2, p, h3, span, table, thead, tr, th, tbody, td)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, stopPropagationOn)

import Shared.Icons.X exposing (heroiconX)
import Shared.Icons.Guide exposing (heroiconGuide)

import Json.Decode as Decode
import Html exposing (i)
import Html exposing (sub)

stopPropagationClick : msg -> Html.Attribute msg
stopPropagationClick noOpMsg =
    stopPropagationOn "click" <|
        Decode.succeed ( noOpMsg, True )

viewGuideModal : msg -> (Int -> msg) -> msg -> Html msg
viewGuideModal onToggleGuideModal onLoadSlot onNoOp =
    div [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
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
                  [ heroiconGuide, text "Random Access Machine Guide" ]
            
            , div [ class "my-1 text-sm" ]
                [ p [ class "mb-2" ]
                    [ 
                        text "Random Access Machine is a computational model similar to conventional sequential computers. It consists of data memory (implemented as registers) and program memory containing instructions. An input/output unit interfaces with the environment, while an arithmetic-logic and control unit interprets and executes the program." 
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
                  [ -- Example 1
                        div [ class "border p-2 rounded bg-white shadow-sm w-full" ]
                              [ div [ class "font-semibold text-gray-700" ]
                                    [ text "Example 1: " 
                                    , span [ class "font-normal" ] [ text "Avg of two numbers" ]
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
                                    , span [ class "font-normal" ] [ text "a^n" ]
                                    ]
                              , div [ class "flex flex-row-reverse gap-2 mt-2" ]
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

            , p [ class "mb-1 text-sm" ]
                  [
                        text "Program memory consists of numbered memory locations where instructions are stored. A label is a special marker in form of numeric constant, that the program uses to remember a specific location. Because labels are not executable commands, they do not count as instructions. For example, a label can be written as "
                        , span [ class "font-bold text-gray-600" ] [ text "jumpHere:" ]
                        , text " ."
                  ]
            , p [ class "mb-2 text-sm" ]
                [ 
                  text "Parsing errors do not count as instructions. However, instructions that trigger a runtime error are counted (including their computational complexity) because the simulator expends effort to determine that they are invalid." 
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
                        
                        -- LOAD instruction row
                        tr [ class "bg-blue-100/70" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-blue-500 font-semibold" ]
                                        [ text "LOAD operand" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "LOAD =10" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Operand is loaded into the accumulator." ]
                            ]
                        
                        -- STORE instruction row
                        , tr [ class "bg-blue-100/70" ]
                              [ td [ class "px-3 py-1 whitespace-normal" ]
                                    [ span [ class "text-blue-500 font-semibold" ]
                                          [ text "STORE operand" ]
                                    ]
                              , td [ class "px-3 py-1 whitespace-normal" ]
                                    [ text "STORE 15" ]
                              , td [ class "px-3 py-1 whitespace-normal" ]
                                    [ text "Content of accumulator is stored into memory." ]
                              ]
                        -- ADD instruction row
                      , tr [ class "bg-blue-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-blue-500 font-semibold" ]
                                        [ text "ADD operand" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "ADD *5" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Operand is added to the accumulator." ]
                            ]
                        -- SUB instruction row
                      , tr [ class "bg-blue-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-blue-500 font-semibold" ]
                                        [ text "SUB operand" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "SUB =1" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Operand is subtracted from the accumulator." ]
                            ]
                        -- MUL instruction row
                      , tr [ class "bg-blue-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-blue-500 font-semibold" ]
                                        [ text "MUL operand" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "MUL 8" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Accumulator is multiplied by the operand." ]
                            ]
                        -- DIV instruction row
                      , tr [ class "bg-blue-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-blue-500 font-semibold" ]
                                        [ text "DIV operand" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "DIV =2" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Accumulator is divided by the operand." ]
                            ]
                        -- READ instruction row
                      , tr [ class "bg-blue-100/70" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-blue-500 font-semibold" ]
                                        [ text "READ operand" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "READ 3" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Value from input tape is stored into operand." ]
                            ]
                        -- WRITE instruction row
                      , tr [ class "bg-blue-100/70" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-blue-500 font-semibold" ]
                                        [ text "WRITE operand" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "WRITE *20" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Accumulator value is stored on the writing tape." ]
                            ]
                        -- JUMP instruction row
                      , tr [ class "bg-gray-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-gray-500 font-semibold" ]
                                        [ text "JUMP label" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "JUMP loop" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Value of the instruction pointer (IP) is set according to the label." ]
                            ]
                        -- JZERO instruction row
                      , tr [ class "bg-gray-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-gray-500 font-semibold" ]
                                        [ text "JZERO label" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "JZERO loop" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "If the accumulator is 0, IP is set according to the label." ]
                            ]
                        -- JGTZ instruction row
                      , tr [ class "bg-gray-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-gray-500 font-semibold" ]
                                        [ text "JGTZ label" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "JGTZ loop" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "If the accumulator is greater than 0, IP is set according to the label." ]
                            ]
                        -- HALT instruction row
                      , tr [ class "bg-yellow-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "text-yellow-500 font-semibold" ]
                                        [ text "HALT" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "HALT" ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Program execution is halted." ]
                            ]
                      ]
                  ]
              
            -- Operands Section
            , h3 [ class "text-lg font-semibold mt-5" ]
                  [ text "Operands:" ]

             , p [ class "mb-2 text-sm" ]
                [ 
                    text "" 
                ]

              , table [ class "min-w-full table-fixed my-2 text-sm" ]
                  [ thead []
                      [ tr []
                          [ th [ class "w-1/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                                [ text "Operand" ]
                          , th [ class "w-1/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                                [ text "Example" ]
                          , th [ class "w-4/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                                [ text "Description" ]
                          ]
                      ]
                  , tbody [ class "bg-white divide-y divide-gray-300" ]
                      [ -- Constant addressing row
                        tr [ class "bg-blue-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "font-bold text-blue-500" ]
                                        [ text "=i" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "ADD =-5" ]
                                  
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Constant addressing. In this example, the constant -5 is added to the accumulator." ]
                            ]
                        -- Direct addressing row
                      , tr [ class "bg-blue-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "font-bold text-blue-500" ]
                                        [ text "i" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "ADD 4" ]

                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Direct addressing. The operand is evaluated as the content of register i. In this example, the value from register 4 is added to the accumulator." ]
                            ]
                        -- Indirect addressing row
                      , tr [ class "bg-blue-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "font-bold text-blue-500" ]
                                        [ text "*i" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "ADD *7" ]

                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "Indirect addressing. The content of register i determines the register whose content is used as the operand. In this example, the value from the register whose index is specified by the content of register 7 is added to the accumulator." ]
                            ]
                      
                      -- label
                      , tr [ class "bg-gray-50" ]
                            [ td [ class "px-3 py-1 whitespace-normal" ]
                                  [ span [ class "font-semibold text-gray-500" ]
                                        [ text "label" ]
                                  ]
                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "jumpHere" ]

                            , td [ class "px-3 py-1 whitespace-normal" ]
                                  [ text "A label is a special type of operand that serves as a numeric constant. Jump instructions use labels to specify the memory location number to which the instruction pointer should jump during execution." ]
                            ]  

                      ]
                  ]

                  
            
            -- complexity
            , h3 [ class "text-lg font-semibold mt-5" ]
                [ text "Computational complexity:" ]
            
            , p [ class "text-sm" ]
                [ 
                    text "Time or space complexity is a significant criterion for evaluating the quality of a program. The RAM machine was designed so that a simulated computation on it corresponds to real-world computers. As a result, complexity measures defined for a RAM program can offer valuable insight, closely correlating with the complexities of real applications." 
                ]
            , p [ class "my-2 text-sm" ]
                  [
                        text "Time complexity refers to the computational cost of a program and indicates how much “time” is required for it to run. Meanwhile, space complexity determines the program’s memory usage. For the RAM machine, there are two complexity measures: the "
                        , span [ class "font-bold" ] [ text "unit cost" ]
                        , span [] [ text " measure and the "]
                        , span [ class "font-bold" ] [ text "logarithmic cost"]
                        , span [] [ text " measure."]
                  ]
            , p [ class "mb-1 text-md font-semibold" ]
                  [
                        text "Unit cost"
                  ]
            , p [ class "mb-2 text-sm" ]
                  [
                        text "Under the unit cost measure, every instruction requires exactly 1 time unit to execute, and each register you use consumes 1 memory unit. So for any given input, the "
                        , span [ class "font-bold" ] [ text "time complexity"]
                        , span [] [ text " is simply the "]
                        , span [ class "font-bold" ] [ text "total number of instructions executed"]
                        , span [] [ text ", and the "]
                        , span [ class "font-bold" ] [ text "space complexity"]
                        , span [] [ text " is the "]
                        , span [ class "font-bold" ] [ text "total number of registers used"]
                        , span [] [ text ". The unit cost measure does not account for the size of the numbers being manipulated or stored in those registers."]
                  ]
            , p [ class "mb-1 text-md font-semibold" ]
                  [
                        text "Logarithmic cost"
                  ]
            , p [ class "text-sm" ]
                  [
                        text "The logarithmic cost measure can be more realistic in certain scenarios because it considers the limited capacity for storing numbers in a real computer’s memory. The larger the number an instruction processes, the “more expensive” it becomes—requiring extra time and memory to store and handle that value. The cost grows "
                        , span [ class "font-bold" ] [ text "logarithmically"]
                        , span [] [ text " with the size of the number. The complexity of manipulating a number "]
                        , i [ class "font-bold" ] [ text "i"]
                        , span [] [ text " is given by the following function:"]
                  ]
            , div 
                    []
                    [ text "$$ l(i) = \\begin{cases} \\lfloor \\log_{2}(\\lvert i \\rvert) \\rfloor + 1, & \\text{if } i \\neq 0,\\\\ 1, & \\text{if } i = 0 \\end{cases} $$"
                    ]
            
            , p [ class "mb-1 text-sm" ]
                  [
                        text "By default a binary logarithm (base of logarithm is 2) is used, so that the function "
                        , i [ class "font-bold"] [ text "l(i)"]
                        , text " corresponds to the number of bits needed to store the integer "
                        , i [ class "font-bold"] [ text "i"]
                        , text ". However, you can select any logarithm base in the simulator’s settings if you prefer a different measure."
                  ]
            
            , p [ class "mb-2 text-sm" ]
                  [
                        text "Using this function, we can define the time cost of working with an operand "
                        , i [class "font-bold"][ text "t(op)" ]
                        , text ", as shown in the following table:"
                  ]
            
            , table [ class "min-w-full table-fixed mt-2 text-sm" ]
                [ thead []
                    [ tr []
                        [ 
                        
                        th [ class "w-1/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                            [ text "Operand"
                            , i [ class "text-xs text-gray-400 lowercase" ] [ text " op" ]
                              ]
                        
                        , th [ class "w-1/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                            [ text "Price" 
                            , i [ class "text-xs text-gray-400 lowercase" ] [ text " t(op)" ]
                            ]
                        
                        , th [ class "w-4/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                            [ text "Description" ]
                        ]
                    ]
                
                , tbody [ class "bg-white divide-y divide-gray-300" ]
                    [     
                    
                    tr [ class "bg-lime-50" ]
                        
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-lime-600 font-bold" ]
                                [ text "=i" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ i [  class "font-semibold" ] [ text "l(i)" ] ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "The cost is determined by how difficult it is to manipulate the number "
                              , i [ class "font-bold" ] [ text "i" ]
                              , text "."
                              ]
                        ]
                    
                    , tr [ class "bg-lime-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-lime-600 font-bold" ]
                                [ text "i" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ i [ class "font-semibold" ] [ text "l(i) + l(c(i))" ] ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "The cost depends on the complexity of handling register index " 
                              , i [ class "font-bold" ] [ text "i" ]
                              , text " (higher indices incur a higher access cost) and on the value stored in register "
                              , i [ class "font-bold" ] [ text "i" ]
                              , text "."
                            ]
                        ]
                    
                    , tr [ class "bg-lime-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                            [ span [ class "text-lime-600 font-bold" ]
                                [ text "*i" ]
                            ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ i [ class "font-semibold" ] [ text "l(i) + l(c(i)) + l(c(c(i)))" ] ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                            [ text "You must “pay” for accessing register "
                            , i [ class "font-bold" ] [ text "i"]
                            , text ", then for accessing register "
                            , i [ class "font-bold" ] [ text "c(i)"]
                            , text " (which is determined by indirect addressing through "
                            , i [ class "font-bold" ] [ text "i"]
                            , text "), and finally for manipulating the number "
                            , i [ class "font-bold" ] [ text "c(c(i))"]
                            , text " stored in register "
                            , i [ class "font-bold" ] [ text "c(i)"]
                            , text "." ]
                        ]
                  ]
            ]
            
            , i [class "font-semibold text-xs text-gray-400"][text "c(i)" ]
            , span [class "text-gray-400 text-xs"][ text " denotes the content of register "]
            , i [ class "font-semibold text-gray-400 text-xs" ] [ text "i" ]

            , p [ class "mt-3 text-sm" ]
                [ 
                    text "Now we can define instruction costs based on the type and size of their operands, as shown in the following table:" 
                ]
            
            , table [ class "min-w-full table-fixed mt-2 text-sm" ]
                  [ thead []
                  [ tr []
                        [ 
                        
                        th [ class "w-1/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                              [ text "Instruction"
                              ]
                        
                        , th [ class "w-1/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                              [ text "Operand" 
                              ]
                        
                        , th [ class "w-4/6 px-3 py-1 bg-gray-100 text-left text-xs font-medium text-gray-600 uppercase tracking-wider" ]
                              [ text "Price" ]
                        ]
                  ]
                
                  , tbody [ class "bg-white divide-y divide-gray-300" ]
                        [     
                    
                        tr [ class "bg-lime-100" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "LOAD" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "op" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "t(op)" ]
                              ]
                        ]

                        , tr [ class "bg-lime-100" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "STORE" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "i" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "l(c(0)) + l(i)" ]
                              ]
                        ]

                        , tr [ class "bg-lime-100" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "STORE" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "*i" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "l(c(0)) + l(i) + l(c(i))" ]
                              ]
                        ]

                        , tr [ class "bg-lime-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "ADD" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "op" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "l(c(0)) + t(op)" ]
                              ]
                        ]

                        , tr [ class "bg-lime-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "SUB" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "op" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "l(c(0)) + t(op)" ]
                              ]
                        ]

                        , tr [ class "bg-lime-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "MUL" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "op" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "l(c(0)) + t(op)" ]
                              ]
                        ]

                        , tr [ class "bg-lime-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "DIV" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "op" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "l(c(0)) + t(op)" ]
                              ]
                        ]

                        , tr [ class "bg-lime-100" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "READ" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "i" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "l(input) + l(i)" ]
                              ]
                        ]

                        , tr [ class "bg-lime-100" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "READ" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "*i" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "l(input) + l(i) + l(c(i))" ]
                              ]
                        ]

                        , tr [ class "bg-lime-100" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "WRITE" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "op" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "t(op)" ]
                              ]
                        ]

                        , tr [ class "bg-lime-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "JUMP" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "label" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "font-semibold" ] [ text "1" ]
                              ]
                        ]

                        , tr [ class "bg-lime-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "JZERO" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "label" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "l(c(0))" ]
                              ]
                        ]

                        , tr [ class "bg-lime-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "JGTZ" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "label" ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ i [ class "font-semibold" ] [ text "l(c(0))" ]
                              ]
                        ]

                        , tr [ class "bg-lime-100" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-lime-600 font-semibold" ]
                                    [ text "HALT" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text " " ]
                        
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "font-semibold" ] [ text "1" ]
                              ]
                        ]
                  ]
            ]

            , p [ class "mt-3 mb-1 text-sm" ]
                [ 
                    span [ class "font-bold" ] [ text "Logarithmic time complexity" ]
                    , text " is "
                    , span [ class "font-bold" ] [ text"the sum of the costs for all instructions"]
                    , text " executed when processing a given input. If instructions are inside a loop, their costs are counted for each iteration. Because register contents may change during execution, the cost of an instruction in a loop can vary from one iteration to the next."
                ]
            , p [ class "mb-2 text-sm" ]
                [ 
                    span [ class "font-bold" ] [ text "The logarithmic space complexity" ]
                    , text " is defined as the "
                    , span [ class "font-bold" ] [ text "sum of "]
                    , i [ class "font-bold" ] [ text "l(x", sub [ class "text-xs" ] [ text "i" ], text ")"]
                    , span [ class "font-bold" ] [ text " for all registers " ]
                    , text "(including the accumulator), where "
                    , i [ class "font-bold" ] [ text "x", sub [ class "text-xs" ] [ text "i" ] ]
                    , text " is the number with the "
                    , span [ class "font-bold" ] [ text "largest absolute value ever stored in register " ]
                    , i [ class "font-bold" ] [ text "i" ]
                    , text " during the computation."
                ]
              
            -- types of errors
            , h3 [ class "text-lg font-semibold mt-5" ]
                [ text "Possible errors:" ]
            
            , p [ class "mb-2 text-sm" ]
                [ 
                    text "Parsing errors are detected during parsing, which is done after each input. Instructions with parsing errors appear in red, and once the program is started, these errors are printed to the console. Runtime errors occur during execution because they cannot be predicted in advance; these are also logged to the console. Warnings are a special type of errors that arise in specific situations." 
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
                    
                  -- PARSING ERROR - UNKNOWN INSTRUCTION
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
                    
                  -- PARSING ERROR - REFERENCING NON-EXISTING REGISTER
                  , tr [ class "bg-red-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-red-500 font-semibold" ]
                                    [ text "Parsing error" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "Found instruction that is attempting to access a non-existing register." ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "This error occurs when the parser detects a direct operand pointing to a register index that doesn’t exist or is out of range. To prevent it, ensure all register references are within the valid range." ]
                        ]
                    
                  -- PARSING ERROR - DUPLICATED LABELS
                  , tr [ class "bg-red-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-red-500 font-semibold" ]
                                    [ text "Parsing error" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "Found duplicated label" ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "This error occurs when the parser encounters a label that has already been defined. To prevent it, ensure that each label is unique." ]
                        ]
                    
                  -- PARSING ERROR - REFERENCING NON-EXISTING LABEL
                  , tr [ class "bg-red-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-red-500 font-semibold" ]
                                    [ text "Parsing error" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "Found instruction that is referencing non-existing label" ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "This error occurs when the parser encounters an instruction that references a label that doesn’t exist. To prevent it, ensure that all labels are defined before they are referenced." ]
                  ]
                  
                  
                  -- PARSING ERROR - DIV BY ZERO
                  , tr [ class "bg-red-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-red-500 font-semibold" ]
                                    [ text "Parsing error" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "Found instruction that is dividing by zero" ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "This error occurs when the parser detects a division instruction with a constant operand of zero (DIV =0). To prevent it, ensure all constant division operands are non-zero." ]
                        ]
                  
                  --PARSING ERROR - INVALID INSTRUCTIONS
                  , tr [ class "bg-red-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-red-500 font-semibold" ]
                                    [ text "Parsing error" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "Found invalid instruction" ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "This error occurs when the parser encounters an instruction that uses a constant operand in a context that doesn’t make sense, such as STORE =5, READ =7, or WRITE =9. To prevent it, ensure that storing, reading, or writing references a valid register, with direct or indirect operands." ]
                        ]
                  
                  --RUNTIME ERROR - read from the input tape, while no more values are available.
                  , tr [ class "bg-red-100" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-red-600 font-semibold" ]
                                    [ text "Runtime error" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "Instruction attempted to read from the input tape, while no more values are available." ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "This runtime error occurs when an instruction tries to read from the input tape, but no additional values remain. To avoid this, ensure your program provides enough input data or includes logic to handle cases where the tape might be empty before attempting another read." ]
                        ]

                  --RUNTIME ERROR - div by direct/indirect operand zero
                  , tr [ class "bg-red-100" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-red-600 font-semibold" ]
                                    [ text "Runtime error" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "Instruction attempted to divide by zero." ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "This runtime error occurs when a division instruction uses a direct operand referencing a register that contains 0, or an indirect operand that eventually leads to a register holding 0. To avoid this, ensure all registers involved in division are non-zero at runtime, or add logic to handle division by zero before the instruction is executed." ]
                        ]
                  
                  --RUNTIME ERROR - attempted to access a non-existent register.
                  , tr [ class "bg-red-100" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-red-600 font-semibold" ]
                                    [ text "Runtime error" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "Instruction attempted to access a non-existent register." ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "This runtime error occurs when an indirect operand ultimately resolves to a register index that doesn’t exist. To avoid it, ensure all indirect operands resolve to valid registers at runtime, or implement checks to prevent referencing out-of-range indices." ]
                        ]


                  -- warning - maximum number of instant-speed instructions exceeded
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


                  --warning More than 100 runtime errors occurred while running at instant speed.
                  , tr [ class "bg-yellow-50" ]
                        [ td [ class "px-3 py-1 whitespace-normal" ]
                              [ span [ class "text-yellow-500 font-semibold" ]
                                    [ text "Warning" ]
                              ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "More than 100 runtime errors occurred while running at instant speed." ]
                        , td [ class "px-3 py-1 whitespace-normal" ]
                              [ text "This warning appears when the simulator accumulates more than 100 runtime errors in instant-speed mode, usually indicating a potential infinite loop or recurring problem. To address it, lower the execution speed or step through your code to identify and fix the source of these repeated errors." ]
                        ]
                  ]
                  
            ]
            ]
      ]
        
      
