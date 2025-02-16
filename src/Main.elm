module Main exposing (main)

-- import Browser
import Html exposing (div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String
import Time
import Array

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)

import Am.Types.Messages as AmMsg
import Am.Types.Model as AmModel
import Am.Views.View as AmView
import Am.Utils.Update as AmUpdate
import Am.Utils.Init as AmInit
import Am.Utils.AbacusParser as AmParser
import Am.Utils.HelperFunctions as AmHelper

import Ram.Types.Messages as RamMsg 
import Ram.Types.Model as RamModel
import Ram.Views.View as RamView
import Ram.Utils.Update as RamUpdate
import Ram.Utils.Init as RamInit
import Ram.Utils.RamParser as RamParser
import Ram.Utils.HelperFunctions as RamHelper

import Shared.Ports exposing (getItem, gotItem)

-- -- SUBSCRIPTIONS
abacusSubscriptions : AmModel.Model -> Sub AmMsg.Msg
abacusSubscriptions model =
    if model.isRunning then
        let
            defaultSpeed = 1000
            speed =
                Array.get (model.speedIdx - 1) model.speeds
                    |> Maybe.withDefault defaultSpeed
        in
        Time.every (toFloat speed) AmMsg.Tick
    else
        Sub.none


ramSubscriptions : RamModel.Model -> Sub RamMsg.Msg
ramSubscriptions model =
    if model.isRunning then
        let
            defaultSpeed = 1000
            speed =
                Array.get (model.speedIdx - 1) model.speeds
                    |> Maybe.withDefault defaultSpeed
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
            Cmd.batch
                [ getItem "am_current"
                , getItem "ram_current"
                , Cmd.batch <|
                    List.map (\i -> getItem ("am_slot_" ++ String.fromInt i)) (List.range 1 20)
                , Cmd.batch <|
                    List.map (\i -> getItem ("ram_slot_" ++ String.fromInt i)) (List.range 1 20)
                
                , getItem "ram_current_input_tape"
                , Cmd.batch <|
                    List.map (\i -> getItem ("ram_slot_" ++ String.fromInt i ++ "_input_tape")) (List.range 1 20)
                ]
    in
    ( { page = pageFromUrl
      , abacusModel = AmInit.init
      , ramModel = RamInit.init
      , key = key
      }
    , cmdToLoad
    )

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
                            ""
            in
            ( { model | page = newPage }
            , Nav.pushUrl model.key newUrl
            )

        AbacusMsg subMsg ->
            let
                ( newAbacusModel, cmd ) =
                    AmUpdate.update subMsg model.abacusModel
            in
            ( { model | abacusModel = newAbacusModel }
            , Cmd.map AbacusMsg cmd
            )

        RamMsg subMsg ->
            let
                ( newRamModel, cmd ) =
                    RamUpdate.update subMsg model.ramModel
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
            in
            ( { model | page = newPage }, Cmd.none )
        
        GotItem (key, code) ->
            let
                rawCode =
                    Maybe.withDefault "" code
            in
            if String.startsWith "am_" key then
                case key of
                    "am_current" ->
                        let
                            instructions =
                                AmParser.parseInstructions [] [] 0 (String.toList rawCode) False
                            
                            innerAbacusModel = model.abacusModel
                            updatedAbacusModel =
                                { innerAbacusModel
                                    | inputText = rawCode
                                    , instructions = instructions
                                }
                        in
                        ( { model | abacusModel = updatedAbacusModel }
                        , Cmd.map AbacusMsg (AmHelper.requestAddMessages ["Welcome to Abacus Machine Simulator"])
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
                                let
                                    updatedSlots =
                                        Array.set i rawCode model.abacusModel.slots

                                    innerAbacusModel = model.abacusModel
                                    updatedAbacusModel =
                                        {
                                            innerAbacusModel
                                            | slots = updatedSlots
                                        }
                                in
                                ( { model | abacusModel = updatedAbacusModel }, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

            else if String.startsWith "ram_" key then
                case key of
                    "ram_current" ->
                        let
                            (instructions, labels) =
                                RamParser.parseRAM rawCode

                            innerRamModel = model.ramModel    
                            updatedRamModel =
                                {   
                                    innerRamModel
                                    | inputText = rawCode
                                    , instructions = instructions
                                    , labels = labels
                                }
                        in
                        ( { model | ramModel = updatedRamModel }
                        , Cmd.map RamMsg (RamHelper.requestAddMessages ["Welcome to RAM Simulator"])
                        )

                    "ram_current_input_tape" -> 
                        let
                            decodedTape =
                                RamHelper.decodeInputTape rawCode
                        in
                        case decodedTape of
                            Just tape ->
                                let
                                    innerRamModel = model.ramModel
                                    updatedRamModel =
                                        { innerRamModel | inputTape = tape }
                                in
                                ( { model | ramModel = updatedRamModel }, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )
                    _ ->
                        if String.endsWith "_input_tape" key then
                            let
                                maybeSlotIndex = String.dropRight 11 key |> String.dropLeft 9 |> String.toInt
                                maybeTape = RamHelper.decodeInputTape rawCode
                            in
                            case maybeSlotIndex of
                                Just i ->
                                    let
                                        updatedTape = Maybe.withDefault Array.empty maybeTape
                                        updatedSlots = Array.set i updatedTape model.ramModel.slots_input_tapes
                                        innerRamModel = model.ramModel
                                        updatedRamModel =
                                            {
                                                innerRamModel
                                                | slots_input_tapes = updatedSlots
                                            }
                                    in
                                    ( { model | ramModel = updatedRamModel }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )
                        else
                            let
                                maybeSlotIndex =
                                    if String.startsWith "ram_slot_" key then
                                        String.dropLeft 9 key |> String.toInt
                                    else
                                        Nothing
                            in
                            case maybeSlotIndex of
                                Just i ->
                                    let
                                        updatedSlots =
                                            Array.set i rawCode model.ramModel.slots

                                        innerRamModel = model.ramModel
                                        updatedRamModel =
                                            {
                                                innerRamModel
                                                | slots = updatedSlots
                                            }
                                    in
                                    ( { model | ramModel = updatedRamModel }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

            else
                ( model, Cmd.none )


        NoOp ->
            ( model , Cmd.none )




view : Model -> Browser.Document Msg
view model =
    case model.page of
        Landing ->
            { title = "Menu"
            , body =
                [ div [ class "flex items-center justify-center min-h-screen bg-gray-100" ]
                    [ div [ class "flex flex-col gap-6" ]
                        [ button
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