module Views.Instructions exposing (..)
import Types.Messages exposing (Msg)
import Html exposing (Html, div, text)
import Html.Attributes
import Types.Instructions exposing (Instruction(..))


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



