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
import Shared.Icons.Bug exposing (heroiconBug)
import Shared.Icons.Github exposing (heroiconGithub)
import Shared.Icons.Info exposing (heroiconInfo)
import Shared.Icons.Survey exposing (survey)
import Shared.Icons.ElmLogo exposing (elmLogo)
import Shared.Icons.X exposing (heroiconX)
import Shared.Icons.Mail exposing (heroiconMail)
import Shared.Icons.GithubSmall exposing (heroiconGithubSmall)

import Html exposing (Html, div, text, button, img, a, p, h2, span)
import Html.Attributes exposing (class, href, target, rel, src, alt, property)
import Html.Events exposing (onClick, stopPropagationOn)

import String
import Time
import Array
import Task
import Process

import Browser
import Browser.Navigation as Nav

import Url exposing (Url)

import Json.Decode as Decode
import Json.Encode exposing (string)

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
    , aboutModalOpen : Bool
    }


type Msg
    = ChangePage Page
    | AbacusMsg AmMsg.Msg
    | RamMsg RamMsg.Msg
    | UrlChanged Url
    | GotItem (String, Maybe String)
    | DelayedSubToTextArea
    | ToggleAboutModal
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
    , aboutModalOpen = False
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
        
        ToggleAboutModal ->
            ( { model | aboutModalOpen = not model.aboutModalOpen }
            , Cmd.none
            )

view : Model -> Browser.Document Msg
view model =
    case model.page of
        Landing ->
            { title = "HnatkoSim | Home"
            , body =
                [ div 
                    [ class "grid grid-rows-[auto-auto-auto] min-h-screen bg-gray-200 font-mono" ]
                    [ 
                    div 
                        [ class "grid grid-rows-2 h-full" ]
                        [ div [ class "flex items-start justify-left" ][
                            a 
                                [ href "https://www.fiit.stuba.sk"
                                , target "_blank"
                                , rel "noopener noreferrer"
                                , class "sm:m-6"
                                ]
                                [ img
                                    [ src "assets\\fiit.png"
                                    , alt "fiit"
                                    , class "w-[20rem] lg:w-[27rem] hidden sm:block"
                                    ]
                                    []
                                ]

                            , -- Smaller image shown on screens < "sm"
                            a 
                                [ href "https://www.fiit.stuba.sk"
                                , target "_blank"
                                , rel "noopener noreferrer"
                                , class "m-5 sm:m-0"
                                ]
                                [ img
                                    [ src "assets\\fiitsmall.webp"
                                    , alt "fiit"
                                    , class "w-[10rem] block sm:hidden"
                                    ]
                                    []
                                ]
                        ]

                        , div [class "flex items-start justify-center" ][   
                            img
                                [ Html.Attributes.src "assets\\hnatkosim.webp"
                                , Html.Attributes.alt "HnatkoSim"
                                , class "w-[20rem] sm:w-[26rem] md:w-[32rem] lg:w-[37rem] breathe"
                                ]
                                []
                            ]
                        ]
                        
                    
                    , div [ class "flex flex-col items-center justify-center" ]
                        [ -- We use inline-grid and a single column.
                        div [ class "inline-grid w-fit grid-cols-1 gap-4" ]
                            [ button
                                [ onClick (ChangePage AbacusPage)
                                , class "px-2 sm:px-8 py-4 bg-blue-500 hover:bg-blue-600 text-white font-semibold rounded shadow-lg transition-colors duration-200"
                                ]
                                [ text "Abacus Machine Simulator" ]
                            , button
                                [ onClick (ChangePage RamPage)
                                , class "px-2 sm:px-8 py-4 bg-blue-500 hover:bg-blue-600 text-white font-semibold rounded shadow-lg transition-colors duration-200"
                                ]
                                [ text "Random Access Machine Simulator" ]
                            , button
                                [ onClick NoOp
                                , class 
                                    """
                                    px-2
                                    sm:px-8 py-4
                                    text-white 
                                    font-bold 
                                    
                                    rounded
                                    shadow-xl
                                    bg-gradient-to-r from-red-500 via-blue-500 to-orange-500 
                                    

                                    hover:animate-none
                                    helicopter-rotate
                                    flex items-center justify-center gap-2

                                    hover:bg-gradient-to-r hover:from-red-600 hover:via-blue-600 hover:to-orange-600
                                    
                                    
                                    opacity-50
                                    cursor-not-allowed
                                    disabled
                                    """
                                ]
                                [ survey, text "SHORT FEEDBACK SURVEY" ]
                            

                                , p 
                                    [ class """
                                        text-xs 
                                        text-gray-500 
                                        text-center 
                                        whitespace-normal 
                                        break-words 
                                        sm:max-w-[50ch]
                                        max-w-[40ch]
                                    """ ]
                                    [ text "*Survey is being prepared and will be available soon. You'll be notified once it's ready!" ]
                            ]
                        ]



                    , div [ class "content-end m-5" ]
                        [
                        div [class "flex flex-col sm:flex-row gap-4"]
                            [
                            button
                                [ onClick ToggleAboutModal
                                , class "sm:w-1/3 px-4 py-2 font-semibold rounded shadow-lg transition-colors duration-200 border border-blue-500 text-blue-500 bg-white hover:bg-blue-50  focus:outline-none flex items-center justify-center gap-2"
                                ]
                                [ heroiconInfo, text "About" ]
                            , a
                                [ href "https://docs.google.com/forms/d/e/1FAIpQLSe4agj2hvxDIDLTRTOW5YYO4QNrZwihH4uf5q9CBlslGeUrAg/viewform?usp=dialog"
                                , target "_blank"
                                , rel "noopener noreferrer"
                                , class "sm:w-1/3 px-4 py-2 font-semibold rounded shadow-lg transition-colors duration-200 border border-blue-500 text-blue-500 bg-white hover:bg-blue-50  focus:outline-none flex items-center justify-center gap-2"
                                ]
                                [ heroiconBug, text "Report a bug" ]
                            , a
                                [ href "https://github.com/martinhnatko/am-ram-simulators"
                                , target "_blank"
                                , rel "noopener noreferrer"
                                , class "sm:w-1/3 px-4 py-2 font-semibold rounded shadow-lg transition-colors duration-200 border border-blue-500 text-blue-500 bg-white hover:bg-blue-50  focus:outline-none flex items-center justify-center gap-2"
                                ]
                                [ heroiconGithub, text "Source code" ]
                            ]
                        ]
                    ]
                
                , ( if model.aboutModalOpen then
                        viewAboutModal
                    else
                        text ""
                    )
                ]

            }

        AbacusPage ->
            AmView.view model.abacusModel
                |> documentMap AbacusMsg

        RamPage ->
            RamView.view model.ramModel
                |> documentMap RamMsg


stopPropagationClick : msg -> Html.Attribute msg
stopPropagationClick noOpMsg =
    stopPropagationOn "click" <|
        Decode.succeed ( noOpMsg, True )

viewAboutModal : Html Msg
viewAboutModal =
    div [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
        , onClick ToggleAboutModal
        ]
        [ div [ class "bg-white p-4 rounded shadow-lg relative max-h-[80vh] max-w-[80vw] md:max-w-[65vw] lg:max-w-[50vw] xl:max-w-[40vw] overflow-y-auto"
                , stopPropagationClick NoOp
                ]
            [ -- Close Button (top-right)
              button
                [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700 focus:outline-none"
                , onClick ToggleAboutModal
                ]
                [ heroiconX ]

            , h2 [ class "text-xl font-bold mb-2 flex items-center gap-1" ] [ heroiconInfo, text "About" ]
 
            , p [ class "mb-0.5 flex items-center gap-1" ] 
                [ text "Author: Martin Hnatko"
                , a 
                    [
                        href "mailto:xhnatko@stuba.sk"
                        , target "_blank"
                        , rel "noopener noreferrer"
                        , class "text-blue-500 hover:text-blue-600"
                    ]
                    [ heroiconMail ]
                , a [
                        href "https://github.com/martinhnatko"
                        , target "_blank"
                        , rel "noopener noreferrer"
                        , class "flex items-center gap-1 text-blue-500 hover:text-blue-600"      
                    ] 
                    [heroiconGithubSmall] 
                ]
            , p [ class "flex flex-row flex-wrap gap-1 items-center justify-start" ] 
                    [ 
                        text "Built in"
                        , a [ href "https://elm-lang.org/"
                            , target "_blank"
                            , rel "noopener noreferrer"
                            , class "flex items-center gap-1 text-blue-500 font-semibold hover:text-blue-600"  
                            ] 
                            [ text "Elm", elmLogo ]
                        , text "as part of a bachelor's thesis project." 
                    ]
                
            , p [ class "mt-3 text-center text-xs text-gray-500" ]
                [ 
                    span [ property "innerHTML" (string "&nbsp;") ] []
                    , text " HnatkoSim, 2025" 
                ]
        
            ]
        ]


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