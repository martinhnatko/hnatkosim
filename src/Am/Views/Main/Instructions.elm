module Am.Views.Main.Instructions exposing (..)
import Am.Types.Messages exposing (Msg)
import Html exposing (Html, div, text)
import Html.Attributes
import Am.Types.Instructions exposing (Instruction(..))
import Html.Attributes exposing (class)


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
                                " bg-gray-200 text-gray-800"

                            EndLoop _ _ ->
                                " bg-gray-200 text-gray-800"

                            UnknownInstruction ->
                                " bg-yellow-200 text-yellow-800"

                    -- Base classes: same border thickness & style for everyone
                    baseClasses =
                        "p-1 border-4 border-solid rounded font-mono transition-colors"

                    -- If active, show a blue border & bold text; otherwise a transparent border
                    activeClasses =
                        if isActive then
                            " border-blue-500 font-bold"
                        else
                            " border-transparent"
                in
                div
                    [ class
                        ("flex items-center gap-2 font-mono " ++ baseClasses ++ typeColorClasses ++ activeClasses
                            -- ++ highlightClass
                        )
                    ]
                    [ div [ class "text-gray-400" ]
                        [ text (String.fromInt (index + 1)) ]

                    , div [ class "h-5 w-px bg-gray-400" ] []

                    , div [] [ text instructionText ]
                    ]
                
                -- div
                --     [ class (baseClasses ++ typeColorClasses ++ activeClasses) ]
                --     [ text instructionText ]
            )
        )



