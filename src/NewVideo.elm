module NewVideo exposing (Error, NewVideo, empty, encode, setTranscript, setVideoId, validate, view)

import ExampleData
import File exposing (File)
import Html exposing (Html, a, article, button, div, h2, input, label, text)
import Html.Attributes exposing (class, for, href, id, rel, target, type_)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Parser exposing (DeadEnd, Problem(..))
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
    , setNewVideoTranscriptFile : File -> msg
    , submitNewVideo : msg
    , newVideoError : Maybe Error
    }


view : ViewProps msg -> Html msg
view props =
    article [ class "w-full max-w-2xl flex flex-col gap-4" ]
        [ a
            [ href "https://docs.invidious.io/instances/"
            , target "_blank"
            , rel "noopener noreferrer"
            , class "bg-blue-600 rounded-lg py-2 px-4 text-center"
            ]
            [ text "Find videos on Invidious" ]
        , h2 [ class "text-2xl" ] [ text "Add a new video" ]
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
            [ label [ class "text-xs", for "new-video-transcript-file" ]
                [ text "New video transcript" ]
            , input
                [ id "new-video-transcript-file"
                , class "bg-slate-700 p-2 rounded-lg"
                , type_ "file"
                , on "change" (Decode.map props.setNewVideoTranscriptFile fileDecoder)
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
                                deadEndsToString deadEnds
                    ]
        ]


fileDecoder : Decoder File
fileDecoder =
    Decode.at [ "target", "files", "0" ] File.decoder


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map
            (\deadEnd ->
                "Error at row: "
                    ++ String.fromInt deadEnd.row
                    ++ ", column: "
                    ++ String.fromInt deadEnd.col
                    ++ ", problem: "
                    ++ problemToString deadEnd.problem
            )
        |> String.join "\n"


problemToString : Problem -> String
problemToString problem =
    case problem of
        Expecting str ->
            "Expecting " ++ str

        ExpectingInt ->
            "Expecting an integer"

        ExpectingHex ->
            "Expecting a hexadecimal number"

        ExpectingOctal ->
            "Expecting an octal number"

        ExpectingBinary ->
            "Expecting a binary number"

        ExpectingFloat ->
            "Expecting a floating point number"

        ExpectingNumber ->
            "Expecting a number"

        ExpectingVariable ->
            "Expecting a variable"

        ExpectingSymbol str ->
            "Expecting a symbol " ++ str

        ExpectingKeyword str ->
            "Expecting a keyword " ++ str

        ExpectingEnd ->
            "Expecting an ending"

        UnexpectedChar ->
            "Unexpected character"

        Problem str ->
            str

        BadRepeat ->
            "Bad repeat"
