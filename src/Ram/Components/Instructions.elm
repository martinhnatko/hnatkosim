module Ram.Components.Instructions exposing (viewInstructions)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Ram.Types.Instructions exposing (Instruction(..), Operand(..))

import String

viewInstructions : List Instruction -> Int -> Bool -> Bool -> Html msg
viewInstructions instructions pointer simStarted halted =
    div [ class ( "flex flex-col gap-1 w-1/3 p-1 shadow-lg rounded overflow-auto border-2 border-transparent " 
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

                    (instructionText, typeColorClasses, maybeExeCount) =
                        case instruction of
                            Load operand isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Load " ++ operandToString operand, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Load " ++ operandToString operand, " bg-blue-200 text-blue-800", Just exeCount)

                            Store operand isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Store " ++ operandToString operand, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Store " ++ operandToString operand, " bg-blue-200 text-blue-800", Just exeCount)

                            Add operand isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Add " ++ operandToString operand, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Add " ++ operandToString operand, " bg-blue-200 text-blue-800", Just exeCount)

                            Sub operand isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Sub " ++ operandToString operand, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Sub " ++ operandToString operand, " bg-blue-200 text-blue-800", Just exeCount)

                            Mul operand isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Mul " ++ operandToString operand, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Mul " ++ operandToString operand, " bg-blue-200 text-blue-800", Just exeCount)

                            Div operand isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Div " ++ operandToString operand, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Div " ++ operandToString operand, " bg-blue-200 text-blue-800", Just exeCount)

                            Read operand isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Read " ++ operandToString operand, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Read " ++ operandToString operand, " bg-blue-200 text-blue-800", Just exeCount)

                            Write operand isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Write " ++ operandToString operand, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Write " ++ operandToString operand, " bg-blue-200 text-blue-800", Just exeCount)

                            Jump _ label isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Jump " ++ label, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Jump " ++ label, " bg-gray-200 text-gray-800", Just exeCount)

                            Jzero _ label isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Jzero " ++ label, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Jzero " ++ label, " bg-gray-200 text-gray-800", Just exeCount)

                            Jgtz _ label isError exeCount ->
                                case isError of
                                    Just _ ->
                                        ("Jgtz " ++ label, " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        ("Jgtz " ++ label, " bg-gray-200 text-gray-800", Just exeCount)

                            Halt exeCount ->
                                ("Halt", " bg-yellow-200 text-yellow-800", Just exeCount)

                            Label lbl isError ->
                                case isError of
                                    Just _ ->
                                        (lbl ++ ":", " bg-red-200 text-red-800", Nothing)
                                    _ ->
                                        (lbl ++ ":", " bg-gray-200 text-gray-800", Nothing)

                            UnknownInstruction ->
                                ("Unknown", " bg-red-200 text-red-800", Nothing)

                    baseClasses =
                        "p-0.5 border-4 border-solid rounded font-mono transition-colors"

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
                    , case maybeExeCount of
                        Just exeCount ->
                            div [ class "text-gray-400 ml-auto mr-2" ]
                                [ text (String.fromInt exeCount ++ "x") ]
                        Nothing ->
                            div [] []
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
