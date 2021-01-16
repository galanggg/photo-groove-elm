module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, p, small, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


type alias Model =
    { photos : List Photo
    , selectedImage : String
    , chosenSize : ThumbnailSize
    }


type alias Photo =
    { url : String }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurprise


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



-- getPhotoUrl : Int -> String
-- getPhotoUrl index =
--     case Array.get index photoArray of
--         Just photo ->
--             photo.url
--         Nothing ->
--             ""


update msg model =
    case msg of
        ClickedPhoto url ->
            { model | selectedImage = url }

        ClickedSize size ->
            { model | chosenSize = size }

        ClickedSurprise ->
            { model | selectedImage = "2.jpeg" }


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurprise ]
            [ text "Random !" ]
        , h3 [] [ text "Thumbnail Size: " ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map
                (viewThumbnail model.selectedImage)
                model.photos
            )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedImage)
            ]
            []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    div []
        [ img
            [ src (urlPrefix ++ thumb.url)
            , classList [ ( "selected", selectedUrl == thumb.url ) ]
            , onClick (ClickedPhoto thumb.url)
            ]
            []
        ]


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedImage = "1.jpeg"
    , chosenSize = Medium
    }


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
