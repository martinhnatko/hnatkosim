module Am.Components.Instructions exposing (..)

import Am.Types.Instructions exposing (Instruction(..))

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)


viewInstructions : List Instruction -> Int -> Bool -> Html msg
viewInstructions instructions pointer simStarted =
    div [ class ( "md:w-1/3 p-1 shadow-lg rounded md:overflow-auto border-2 border-transparent flex flex-wrap gap-1 content-start" 
            ++ if (pointer >= List.length instructions) && simStarted then
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
                                Increment n isError ->
                                    case isError of
                                        Just _ ->
                                            ("Add " ++ String.fromInt n, " bg-red-200 text-red-800")
                                        _ ->
                                            ("Add " ++ String.fromInt n, " bg-green-200 text-green-800")

                                Decrement n isError ->
                                    case isError of
                                        Just _ ->
                                            ("Sub " ++ String.fromInt n, " bg-red-200 text-red-800")
                                        _ ->
                                            ("Sub " ++ String.fromInt n, " bg-yellow-200 text-yellow-800")

                                StartLoop _ _ isError ->
                                    case isError of
                                        Just _ ->
                                            ("(",  " bg-red-200 text-red-800")
                                        _ ->
                                            ("(",  " bg-gray-200 text-gray-800")

                                EndLoop _ conditionIndex isError ->
                                    case isError of
                                        Just _ ->
                                            (")" ++ String.fromInt conditionIndex, " bg-red-200 text-red-800")
                                        _ ->
                                            (")" ++ String.fromInt conditionIndex, " bg-gray-200 text-gray-800")

                                UnknownInstruction ->
                                    ("Unknown", " bg-red-200 text-red-800")

                        -- Base classes: same border thickness & style for everyone
                        baseClasses =
                            "p-0.5 border-2 border-solid rounded font-mono transition-colors"

                        -- If active, show a blue border & bold text; otherwise a transparent border
                        activeClasses =
                            if isActive then
                                " border-blue-500 font-bold"
                            else
                                " border-transparent"
                    in
                    div
                        [ class
                            ("flex items-center gap-1.5 font-mono " ++ baseClasses ++ typeColorClasses ++ activeClasses)
                        , id (String.fromInt index)
                        ]
                        [ div [ class "text-gray-400" ]
                            [ text (String.fromInt (index + 1)) ]

                        , div [ class "h-5 w-px bg-gray-400" ] []

                        , div [] [ text instructionText ]
                        ]
                )
            )