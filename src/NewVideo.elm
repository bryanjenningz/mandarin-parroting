module NewVideo exposing (Error(..), NewVideo, empty, view)

import Html exposing (Html, article, button, div, h2, input, label, text, textarea)
import Html.Attributes exposing (for, id)
import Html.Events exposing (onClick, onInput)


type alias NewVideo =
    { newVideoId : String
    , newVideoTranscript : String
    }


empty : NewVideo
empty =
    { newVideoId = ""
    , newVideoTranscript = ""
    }


type alias ViewProps msg =
    { setNewVideoId : String -> msg
    , setNewVideoTranscript : String -> msg
    , submitNewVideo : msg
    }


view : ViewProps msg -> Html msg
view props =
    article []
        [ h2 [] [ text "Add a new video" ]
        , div []
            [ label [ for "new-video-id-input" ]
                [ text "New video ID" ]
            , input
                [ id "new-video-id-input"
                , onInput props.setNewVideoId
                ]
                []
            ]
        , div []
            [ label [ for "new-video-transcript-textarea" ]
                [ text "New video transcript" ]
            , textarea
                [ id "new-video-transcript-textarea"
                , onInput props.setNewVideoTranscript
                ]
                []
            ]
        , button [ onClick props.submitNewVideo ]
            [ text "Add video" ]
        ]


type Error
    = EmptyVideoId
    | InvalidVideoId
    | EmptyTranscript
    | InvalidTranscript
