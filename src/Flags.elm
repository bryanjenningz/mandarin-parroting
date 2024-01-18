module Flags exposing (Flags, decode, encode)

import Flashcard exposing (Flashcard)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import ProgressBar exposing (ProgressBar)
import Video exposing (Video, VideoId)


type alias Flags =
    { videoId : Maybe VideoId
    , videoSpeed : Int
    , videos : List Video
    , flashcards : List Flashcard
    , progressBar : ProgressBar
    }


decode : Value -> Flags
decode value =
    value
        |> Decode.decodeValue Decode.string
        |> Result.andThen (Decode.decodeString flagsDecoder)
        |> Result.withDefault flagsDefault


encode : Flags -> Encode.Value
encode flags =
    Encode.object
        [ ( "videoId"
          , flags.videoId
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        , ( "videoSpeed", Encode.int flags.videoSpeed )
        , ( "videos", Encode.list Video.encoder flags.videos )
        , ( "flashcards", Encode.list Flashcard.encoder flags.flashcards )
        , ( "progressBar", ProgressBar.encoder flags.progressBar )
        ]



-- INTERNAL


flagsDefault : Flags
flagsDefault =
    { videoId = Nothing
    , videoSpeed = 100
    , videos = []
    , flashcards = []
    , progressBar = ProgressBar.init
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map5
        (\videoId videoSpeed videos flashcards progressBar ->
            { videoId = videoId
            , videoSpeed = videoSpeed
            , videos = videos
            , flashcards = flashcards
            , progressBar = progressBar
            }
        )
        (Decode.field "videoId" (Decode.maybe Decode.string))
        (Decode.field "videoSpeed" Decode.int)
        (Decode.field "videos" (Decode.list Video.decoder))
        (Decode.field "flashcards" (Decode.list Flashcard.decoder))
        (Decode.field "progressBar" ProgressBar.decoder)
