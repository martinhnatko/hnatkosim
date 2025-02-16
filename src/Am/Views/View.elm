module Am.Views.View exposing (..)
import Am.Types.Model exposing (Model)
import Am.Types.Messages exposing (Msg)
import Html exposing (div)
import Html.Attributes exposing (class)

import Am.Views.HeaderView exposing (headerView)
import Am.Views.MainView exposing (mainContentView)
import Am.Views.FooterView exposing (footerView)
import Am.Views.Header.Menu exposing (viewSlotsModal)
import Html exposing (text)
import Browser


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "AM Simulator"
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