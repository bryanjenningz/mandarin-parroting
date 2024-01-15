module NewVideo exposing (Error, NewVideo, empty, encode, setTranscript, setVideoId, validate, view)

import ExampleData
import Html exposing (Html, article, button, div, h2, input, label, text, textarea)
import Html.Attributes exposing (class, for, id)
import Html.Events exposing (onClick, onInput)
import Parser exposing (DeadEnd)
import Subtitle exposing (Subtitle)
import Video exposing (VideoId)



-- NEW VIDEO


type NewVideo
    = NewVideo
        { newVideoId : String
        , newVideoTranscript : String
        }


empty : NewVideo
empty =
    NewVideo
        { newVideoId = ExampleData.videoId
        , newVideoTranscript = ExampleData.transcript
        }


setVideoId : VideoId -> NewVideo -> NewVideo
setVideoId videoId (NewVideo newVideo) =
    NewVideo { newVideo | newVideoId = videoId }


setTranscript : String -> NewVideo -> NewVideo
setTranscript transcript (NewVideo newVideo) =
    NewVideo { newVideo | newVideoTranscript = transcript }



-- VALIDATE


type Error
    = EmptyVideoId
    | EmptyTranscript
    | InvalidTranscript (List DeadEnd)


type ValidNewVideo
    = ValidNewVideo
        { videoId : String
        , subtitles : List Subtitle
        }


validate : NewVideo -> Result Error ValidNewVideo
validate (NewVideo newVideo) =
    if String.isEmpty (String.trim newVideo.newVideoId) then
        Err EmptyVideoId

    else if String.isEmpty (String.trim newVideo.newVideoTranscript) then
        Err EmptyTranscript

    else
        case Subtitle.fromTranscript newVideo.newVideoTranscript of
            Err deadEnds ->
                Err (InvalidTranscript deadEnds)

            Ok subtitles ->
                Ok (ValidNewVideo { videoId = newVideo.newVideoId, subtitles = subtitles })


encode : ValidNewVideo -> { videoId : String, subtitles : List Subtitle }
encode (ValidNewVideo newVideo) =
    newVideo



-- VIEW


type alias ViewProps msg =
    { setNewVideoId : String -> msg
    , setNewVideoTranscript : String -> msg
    , submitNewVideo : msg
    , newVideoError : Maybe Error
    }


view : ViewProps msg -> Html msg
view props =
    article [ class "w-full max-w-2xl flex flex-col gap-4" ]
        [ h2 [ class "text-2xl" ] [ text "Add a new video" ]
        , div [ class "flex flex-col" ]
            [ label [ class "text-xs", for "new-video-id-input" ]
                [ text "New video ID" ]
            , input
                [ id "new-video-id-input"
                , class "bg-slate-700 p-2 rounded-lg"
                , onInput props.setNewVideoId
                ]
                []
            ]
        , div [ class "flex flex-col" ]
            [ label [ class "text-xs", for "new-video-transcript-textarea" ]
                [ text "New video transcript" ]
            , textarea
                [ id "new-video-transcript-textarea"
                , class "bg-slate-700 p-2 rounded-lg resize-y"
                , onInput props.setNewVideoTranscript
                ]
                []
            ]
        , button
            [ class "bg-blue-600 py-2 px-4 rounded-lg"
            , onClick props.submitNewVideo
            ]
            [ text "Add video" ]
        , case props.newVideoError of
            Nothing ->
                text ""

            Just error ->
                div [ class "text-red-500" ]
                    [ text <|
                        case error of
                            EmptyVideoId ->
                                "Enter a video ID."

                            EmptyTranscript ->
                                "Enter a transcript."

                            InvalidTranscript deadEnds ->
                                Parser.deadEndsToString deadEnds
                    ]
        ]
