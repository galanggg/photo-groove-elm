module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (div, h1, img, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { photos : List Photo
    , selectedImage : String
    , selectedText : String
    }


type alias Photo =
    { url : String, desc : String }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedImage = msg.data, selectedText = msg.data }

    else
        model


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
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
    }


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
