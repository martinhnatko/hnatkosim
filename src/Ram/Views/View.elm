module Ram.Views.View exposing (..)
import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg)
import Html exposing (div)
import Html.Attributes exposing (class)

import Ram.Views.HeaderView exposing (headerView)
import Ram.Views.MainView exposing (mainContentView)
import Ram.Views.FooterView exposing (footerView)
import Ram.Views.Header.Menu exposing (viewSlotsModal)
import Html exposing (text)
import Browser


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "RAM Simulator"
    , body =
        [ div [ class "flex flex-col h-screen p-2 bg-gray-200" ]
            [ -- Header Section
              headerView model

              -- Main Content Section
            , mainContentView model

              -- Footer Section
            , footerView model

              -- Slots Modal
            , if model.showSlotsModal then
                  viewSlotsModal model
              else
                  text ""
            ]
        ]
    }