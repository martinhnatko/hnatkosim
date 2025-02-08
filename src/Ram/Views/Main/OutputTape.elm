module Ram.Views.Main.OutputTape exposing (viewOutputTape)

import Ram.Types.Messages exposing (Msg(..))
import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (class, type_, value, disabled)
import Html.Events exposing (onInput, onClick, onFocus, onBlur)
import String
import Ram.Types.Model exposing (Model)
import Array
import Html.Events exposing (onMouseDown)
import Html.Attributes exposing (id)

import Shared.Icons.XSmall exposing (heroiconX)


viewOutputTape : Model -> Html Msg
viewOutputTape model =
    let
        -- Convert the output tape array to a list and keep only the Just values.
        cells : List Int
        cells =
            model.outputTape
                |> Array.toList
                |> List.filterMap identity
    in
    div [ class "flex rounded bg-white space-x-2 p-3 overflow-x-auto" ]
        (if List.isEmpty cells then
            -- Render a placeholder cell that is invisible but takes up the same space
            [ div [ class "w-20 h-20 border rounded invisible" ] [] ]
         else
            (List.map (\n ->
            div
                [ class "w-20 h-20 border rounded text-center flex items-center justify-center font-mono bg-white" ]
                [ text (String.fromInt n) ]
            )
            cells
            )

        )
        
