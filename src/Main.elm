module Main exposing (main)

import Am.Types.Messages as AmMsg
import Am.Types.Model as AmModel
import Am.Types.Slot as AmSlot
import Am.View as AmView
import Am.Utils.Update as AmUpdate
import Am.Utils.Init as AmInit
import Am.Utils.AbacusParser as AmParser
import Am.Utils.HelperFunctions as AmHelper

import Ram.Types.Messages as RamMsg 
import Ram.Types.Model as RamModel
import Ram.Types.Slot as RamSlot
import Ram.View as RamView
import Ram.Utils.Update as RamUpdate
import Ram.Utils.Init as RamInit
import Ram.Utils.RamParser as RamParser
import Ram.Utils.HelperFunctions as RamHelper

import Shared.Ports exposing (getItem, gotItem, subToTextArea, scrollToBottom)
import Shared.Types.ConsoleMessage exposing (ConsoleMessageType(..))

import Html exposing (div, text, button, img)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import String
import Time
import Array
import Task
import Process

import Browser
import Browser.Navigation as Nav

import Url exposing (Url)

import Json.Decode as Decode


-- SUBSCRIPTIONS
abacusSubscriptions : AmModel.Model -> Sub AmMsg.Msg
abacusSubscriptions model =
    if model.isRunning then
        let
            speed =
                Array.get (model.speedIdx - 1) model.speeds
                    |> Maybe.withDefault 1000
        in
        Time.every (toFloat speed) AmMsg.Tick
    else
        Sub.none


ramSubscriptions : RamModel.Model -> Sub RamMsg.Msg
ramSubscriptions model =
    if model.isRunning then
        let
            speed =
                Array.get (model.speedIdx - 1) model.speeds
                    |> Maybe.withDefault 1000
        in
        Time.every (toFloat speed) RamMsg.Tick
    else
        Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        abacusSubs =
            case model.page of
                AbacusPage ->
                    abacusSubscriptions model.abacusModel
                        |> Sub.map AbacusMsg

                _ ->
                    Sub.none

        ramSubs =
            case model.page of
                RamPage ->
                    ramSubscriptions model.ramModel
                        |> Sub.map RamMsg

                _ ->
                    Sub.none
    in
    Sub.batch
        [ abacusSubs
        , ramSubs
        , gotItem GotItem 
        ]





type Page
    = Landing
    | AbacusPage
    | RamPage



type alias Model =
    { page : Page
    , abacusModel : AmModel.Model
    , ramModel : RamModel.Model
    , key : Nav.Key
    }


type Msg
    = ChangePage Page
    | AbacusMsg AmMsg.Msg
    | RamMsg RamMsg.Msg
    | UrlChanged Url
    | GotItem (String, Maybe String)
    | DelayedSubToTextArea
    | NoOp


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        pageFromUrl =
            case url.fragment of
                Just "abacus" ->
                    AbacusPage

                Just "ram" ->
                    RamPage

                _ ->
                    Landing

        cmdToLoad =
            if pageFromUrl == AbacusPage || pageFromUrl == RamPage then
                Cmd.batch
                    [ getItem "am_current"
                    , getItem "ram_current"
                    , Cmd.batch <|
                        List.map (\i -> getItem ("am_slot_" ++ String.fromInt i)) (List.range 1 20)
                    , Cmd.batch <|
                        List.map (\i -> getItem ("ram_slot_" ++ String.fromInt i)) (List.range 1 20)
                    , Cmd.map AbacusMsg (AmHelper.requestAddMessage (InfoMessage, "Welcome to Abacus Machine Simulator") )
                    , Cmd.map RamMsg (RamHelper.requestAddMessage (InfoMessage, "Welcome to RAM Simulator") )    
                    , subToTextArea ()
                    ]
            else
                Cmd.batch
                    [ getItem "am_current"
                    , getItem "ram_current"
                    , Cmd.batch <|
                        List.map (\i -> getItem ("am_slot_" ++ String.fromInt i)) (List.range 1 20)
                    , Cmd.batch <|
                        List.map (\i -> getItem ("ram_slot_" ++ String.fromInt i)) (List.range 1 20)
                    , Cmd.map AbacusMsg (AmHelper.requestAddMessage (InfoMessage, "Welcome to Abacus Machine Simulator") )
                    , Cmd.map RamMsg (RamHelper.requestAddMessage (InfoMessage, "Welcome to RAM Simulator") )    
                    ]
    in
    ( { page = pageFromUrl
    , abacusModel = AmInit.init
    , ramModel = RamInit.init
    , key = key
    }
    , cmdToLoad
    )

decodeSlotAm : String -> Maybe AmSlot.Slot
decodeSlotAm str =
    let
        decodedSlot = Decode.map2 AmSlot.Slot
                    (Decode.field "name" Decode.string)
                    (Decode.field "inputText" Decode.string)
    in
    Decode.decodeString decodedSlot str |> Result.toMaybe

decodeSlotRam : String -> Maybe RamSlot.Slot
decodeSlotRam str =
    let
        decodedSlot = Decode.map3 RamSlot.Slot
                    (Decode.field "name" Decode.string)
                    (Decode.field "inputText" Decode.string)
                    (Decode.field "inputTape" (Decode.array Decode.int))
    in
    Decode.decodeString decodedSlot str |> Result.toMaybe

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage newPage ->
            let
                newUrl =
                    case newPage of
                        AbacusPage ->
                            "#abacus"

                        RamPage ->
                            "#ram"

                        Landing ->
                            "#"

            in
            ( { model | page = newPage }
            , 
            Nav.pushUrl model.key newUrl
            )

        AbacusMsg subMsg ->
            case subMsg of
                AmMsg.GoBackToMenu ->
                    ( { model | page = Landing }
                    , Nav.pushUrl model.key "#"
                    )
                _ ->
                    let
                        ( newAbacusModel, cmd ) = AmUpdate.update subMsg model.abacusModel
                    in
                    ( { model | abacusModel = newAbacusModel }
                    , Cmd.map AbacusMsg cmd
                    )

        RamMsg subMsg ->
            case subMsg of
                RamMsg.GoBackToMenu ->
                    ( { model | page = Landing }
                    , Nav.pushUrl model.key "#"
                    )
                _ ->
                    let
                        ( newRamModel, cmd ) = RamUpdate.update subMsg model.ramModel
                    in
                    ( { model | ramModel = newRamModel }
                    , Cmd.map RamMsg cmd
                    )

        UrlChanged url ->
            let
                newPage =
                    case url.fragment of
                        Just "abacus" ->
                            AbacusPage

                        Just "ram" ->
                            RamPage

                        _ ->
                            Landing
                
                cmdToLoad =
                    if newPage == AbacusPage || newPage == RamPage then
                        Cmd.batch [ Task.perform (\_ -> DelayedSubToTextArea) (Process.sleep (100)), scrollToBottom "consoleContainer"]
                    else
                        Cmd.none
            in
            ( { model | page = newPage }
             , cmdToLoad
            )
        
        GotItem (key, code) ->
            if String.startsWith "am_" key then
                let
                    decodedSlot = decodeSlotAm (Maybe.withDefault "" code)
                in
                case decodedSlot of
                    Just slot ->
                        case key of
                            "am_current" ->
                                let
                                    instructions = AmParser.parseAM slot.inputText model.abacusModel
                                    
                                    innerAbacusModel = model.abacusModel
                                    updatedAbacusModel =
                                        { innerAbacusModel
                                            | inputText = slot.inputText
                                            , instructions = instructions
                                        }
                                in
                                ( { model | abacusModel = updatedAbacusModel }
                                , Cmd.none
                                )

                            _ ->
                                let
                                    maybeSlotIndex =
                                        if String.startsWith "am_slot_" key then
                                            String.dropLeft 8 key |> String.toInt
                                        else
                                            Nothing
                                in
                                case maybeSlotIndex of
                                    Just i ->
                                        case Array.get i model.abacusModel.slots of
                                            Just updatingThisSlot ->
                                                let
                                                    updatedSlot = { updatingThisSlot | inputText = slot.inputText, name = slot.name }
                                                    innerAbacusModel = model.abacusModel
                                                    updatedAbacusModel = 
                                                        { 
                                                            innerAbacusModel 
                                                            | slots = Array.set i updatedSlot model.abacusModel.slots 
                                                        }

                                                in
                                                ( { model | abacusModel = updatedAbacusModel }
                                                , Cmd.none
                                                )

                                            Nothing ->
                                                ( model, Cmd.none )

                                    Nothing ->
                                        ( model, Cmd.none )
                    Nothing ->
                        ( model, Cmd.none )

            else if String.startsWith "ram_" key then
                let
                    decodedSlot = decodeSlotRam (Maybe.withDefault "" code)
                in
                case decodedSlot of
                    Just slot ->
                        case key of
                            "ram_current" ->
                                let
                                    instructions = RamParser.parseRAM slot.inputText model.ramModel

                                    innerRamModel = model.ramModel    
                                    updatedRamModel =
                                        {   
                                            innerRamModel
                                            | inputText = slot.inputText
                                            , instructions = instructions
                                            , inputTape = slot.inputTape
                                        }
                                in
                                ( { model | ramModel = updatedRamModel }
                                , Cmd.none
                                )

                            _ ->
                                let
                                    maybeSlotIndex =
                                        if String.startsWith "ram_slot_" key then
                                            String.dropLeft 9 key |> String.toInt
                                        else
                                            Nothing
                                in
                                case maybeSlotIndex of
                                    Just i ->
                                        case Array.get i model.ramModel.slots of
                                            Just updatingThisSlot ->
                                                let
                                                    updatedSlots = { updatingThisSlot | inputText = slot.inputText, name = slot.name, inputTape = slot.inputTape }

                                                    innerRamModel = model.ramModel
                                                    updatedRamModel =
                                                        {
                                                            innerRamModel
                                                            | slots = Array.set i updatedSlots model.ramModel.slots
                                                        }
                                                in
                                                ( { model | ramModel = updatedRamModel }
                                                , Cmd.none 
                                                )

                                            Nothing ->
                                                ( model, Cmd.none )

                                    Nothing ->
                                        ( model, Cmd.none )
                    Nothing ->
                            ( model, Cmd.none )

            else
                ( model, Cmd.none )


        NoOp ->
            ( model , Cmd.none )
    
        DelayedSubToTextArea ->
            ( model, subToTextArea () )

view : Model -> Browser.Document Msg
view model =
    case model.page of
        Landing ->
            { title = "HnatkoSim | Menu"
            , body =
                [ div [ class "flex flex-col items-center justify-center min-h-screen bg-gray-200" ]
                    [ 
                    
                    div [class "flex flex-col items-center mb-10"]
                    [
                        img
                            [ Html.Attributes.src "assets\\favicon\\android-chrome-512x512.png"
                            , Html.Attributes.alt "icon"
                            , class "w-11"
                            ]
                            []
                        
                        , img
                            [ Html.Attributes.src "assets/HnatkoSim.gif"
                            , Html.Attributes.alt "HnatkoSim"
                            , class "w-96"  -- Example sizing classes
                            ]
                            []

                    ]
                        
                    , div [ class "flex flex-col gap-5" ]
                        [ 
                            
                        
                        button
                            [ onClick (ChangePage AbacusPage)
                            , class "px-6 py-3 bg-blue-500 hover:bg-blue-600 text-white font-semibold rounded shadow-lg transition-colors duration-200"
                            ]
                            [ text "Abacus Machine Simulator" ]
                        , button
                            [ onClick (ChangePage RamPage)
                            , class "px-6 py-3 bg-blue-500 hover:bg-blue-600 text-white font-semibold rounded shadow-lg transition-colors duration-200"
                            ]
                            [ text "Random Access Machine Simulator" ]
                        ]
                    ]
                ]
            }

        AbacusPage ->
            AmView.view model.abacusModel
                |> documentMap AbacusMsg

        RamPage ->
            RamView.view model.ramModel
                |> documentMap RamMsg

documentMap : (msg -> msg2) -> Browser.Document msg -> Browser.Document msg2
documentMap f doc =
    { title = doc.title
    , body = List.map (Html.map f) doc.body
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = \_ -> NoOp
        }