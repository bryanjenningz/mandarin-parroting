module NewVideo exposing (NewVideo, view)

import Html exposing (Html, article, button, div, h2, input, label, text, textarea)
import Html.Attributes exposing (for, id)


type alias NewVideo =
    { newVideoId : String
    , newVideoTranscript : String
    }


view : Html msg
view =
    article []
        [ h2 [] [ text "Add a new video" ]
        , div []
            [ label [ for "new-video-id-input" ] [ text "New video ID" ]
            , input [ id "new-video-id-input" ] []
            ]
        , div []
            [ label [ for "new-video-transcript-textarea" ] [ text "New video transcript" ]
            , textarea [ id "new-video-transcript-textarea" ] []
            ]
        , button [] [ text "Add video" ]
        ]
