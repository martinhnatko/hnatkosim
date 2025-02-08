module Ram.Utils.HelperFunctions exposing (..)
import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Instructions exposing (Instruction(..))
import Time
import Task
import Ram.Types.Model exposing (Model)
import Dict

requestAddMessages : List String -> Cmd Msg
requestAddMessages msgs =
    msgs
        |> List.map (\msg -> Time.now |> Task.perform (\posix -> AddMessageWithTime posix msg))
        |> Cmd.batch





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


getRegisterValue : Int -> Model -> Int
getRegisterValue regIndex model =
    Dict.get regIndex model.registers |> Maybe.withDefault 0

updateRegister : Int -> Int -> Model -> Model
updateRegister regIndex newValue model =
    { model | registers = Dict.insert regIndex newValue model.registers }

incrementIP : Model -> Model
incrementIP model =
    { model | instructionPointer = model.instructionPointer + 1 }