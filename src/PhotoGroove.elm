module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, p, small, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { photos : List Photo
    , selectedImage : String
    , selectedText : String
    , chosenSize : ThumbnailSize
    }


type alias Photo =
    { url : String, desc : String }


type alias Msg =
    { description : String, data : String }


type ThumbnailSize
    = Small
    | Medium
    | Large


type Maybe value
    = Just value
    | Nothing


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


update msg model =
    case msg.description of
        "ClickedPhoto" ->
            { model | selectedImage = msg.data, selectedText = msg.data }

        "ClickSurprise" ->
            { model | selectedImage = "2.jpeg" }

        _ ->
            model


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick { description = "ClickSurprise", data = "" } ]
            [ text "Random !" ]
        , h3 [] [ text "Thumbnail Size: " ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map
                (viewThumbnail model.selectedImage model.selectedText)
                model.photos
            )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedImage)
            ]
            []
        , h1 [] [ text model.selectedText ]
        ]


viewThumbnail : String -> String -> Photo -> Html Msg
viewThumbnail selectedUrl selectedText thumb =
    div []
        [ img
            [ src (urlPrefix ++ thumb.url)
            , classList [ ( "selected", selectedUrl == thumb.url ) ]
            , onClick { description = "ClickedPhoto", data = thumb.url }
            ]
            []
        , h1 [ onClick { description = "ClickedPhoto", data = thumb.desc } ] []
        ]


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size" ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg", desc = "gambar1" }
        , { url = "2.jpeg", desc = "gambar2" }
        , { url = "3.jpeg", desc = "gambar3" }
        ]
    , selectedImage = "1.jpeg"
    , selectedText = "gambar1"
    , chosenSize = Medium
    }


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
