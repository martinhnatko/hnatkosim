module Ram.Components.Instructions exposing (viewInstructions)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Ram.Types.Instructions exposing (Instruction(..), Operand(..))

import String

viewInstructions : List Instruction -> Int -> Bool -> Bool -> Html msg
viewInstructions instructions pointer simStarted halted =
    div [ class ( "flex flex-col gap-2 w-1/3 p-3 shadow-lg rounded overflow-auto border-2 border-transparent " 
            ++ if ((pointer >= List.length instructions) && simStarted) || halted then
                    " bg-green-50 border-green-400"
                else
                    " bg-white"
            )
        ]
        (instructions
            |> List.indexedMap (\index instruction ->
                let
                    isActive =
                        index == pointer

                    (instructionText, typeColorClasses) =
                        case instruction of
                            Load operand isError ->
                                case isError of
                                    Just _ ->
                                        ("Load " ++ operandToString operand, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Load " ++ operandToString operand, " bg-blue-200 text-blue-800")

                            Store operand isError ->
                                case isError of
                                    Just _ ->
                                        ("Store " ++ operandToString operand, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Store " ++ operandToString operand, " bg-blue-200 text-blue-800")

                            Add operand isError ->
                                case isError of
                                    Just _ ->
                                        ("Add " ++ operandToString operand, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Add " ++ operandToString operand, " bg-blue-200 text-blue-800")

                            Sub operand isError ->
                                case isError of
                                    Just _ ->
                                        ("Sub " ++ operandToString operand, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Sub " ++ operandToString operand, " bg-blue-200 text-blue-800")

                            Mul operand isError ->
                                case isError of
                                    Just _ ->
                                        ("Mul " ++ operandToString operand, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Mul " ++ operandToString operand, " bg-blue-200 text-blue-800")

                            Div operand isError ->
                                case isError of
                                    Just _ ->
                                        ("Div " ++ operandToString operand, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Div " ++ operandToString operand, " bg-blue-200 text-blue-800")

                            Read operand isError ->
                                case isError of
                                    Just _ ->
                                        ("Read " ++ operandToString operand, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Read " ++ operandToString operand, " bg-blue-200 text-blue-800")

                            Write operand isError ->
                                case isError of
                                    Just _ ->
                                        ("Write " ++ operandToString operand, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Write " ++ operandToString operand, " bg-blue-200 text-blue-800")

                            Jump _ label isError ->
                                case isError of
                                    Just _ ->
                                        ("Jump " ++ label, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Jump " ++ label, " bg-gray-200 text-gray-800")

                            Jzero _ label isError ->
                                case isError of
                                    Just _ ->
                                        ("Jzero " ++ label, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Jzero " ++ label, " bg-gray-200 text-gray-800")

                            Jgtz _ label isError ->
                                case isError of
                                    Just _ ->
                                        ("Jgtz " ++ label, " bg-red-200 text-red-800")
                                    _ ->
                                        ("Jgtz " ++ label, " bg-gray-200 text-gray-800")

                            Halt ->
                                ("Halt", " bg-yellow-200 text-yellow-800")

                            Label lbl isError ->
                                case isError of
                                    Just _ ->
                                        (lbl ++ ":", " bg-red-200 text-red-800")
                                    _ ->
                                        (lbl ++ ":", " bg-gray-200 text-gray-800")

                            UnknownInstruction ->
                                ("Unknown", " bg-red-200 text-red-800")

                    baseClasses =
                        "p-1 border-4 border-solid rounded font-mono transition-colors"

                    activeClasses =
                        if isActive then
                            " border-blue-500 font-bold"
                        else
                            " border-transparent"
                    
                in
                div
                    [ class
                        ("flex items-center gap-3 font-mono " ++ baseClasses ++ typeColorClasses ++ activeClasses)
                    ]
                    [ div [ class "text-gray-400 w-8 text-right" ]
                        [ text (String.fromInt (index + 1)) ]

                    , div [ class "h-6 w-px bg-gray-400" ] []

                    , div [] [ text instructionText ]
                    ]
            )
        )

operandToString : Operand -> String
operandToString operand =
    case operand of
        Constant n ->
            "=" ++ String.fromInt n

        Direct n ->
            String.fromInt n

        Indirect n ->
            "*" ++ String.fromInt n
