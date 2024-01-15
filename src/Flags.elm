module Flags exposing (Flags, decode)

import Flashcard exposing (Flashcard)
import Json.Decode as Decode exposing (Decoder, Value)
import Video exposing (Video, VideoId)


type alias Flags =
    { videoId : Maybe VideoId
    , videoSpeed : Int
    , videos : List Video
    , flashcards : List Flashcard
    }


decode : Value -> Flags
decode value =
    value
        |> Decode.decodeValue Decode.string
        |> Result.andThen (Decode.decodeString flagsDecoder)
        |> Result.withDefault flagsDefault



-- INTERNAL


flagsDefault : Flags
flagsDefault =
    { videoId = Nothing
    , videoSpeed = 100
    , videos = []
    , flashcards = []
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map4
        (\videoId videoSpeed videos flashcards ->
            { videoId = videoId
            , videoSpeed = videoSpeed
            , videos = videos
            , flashcards = flashcards
            }
        )
        (Decode.field "videoId" (Decode.maybe Decode.string))
        (Decode.field "videoSpeed" Decode.int)
        (Decode.field "videos" (Decode.list Video.decoder))
        (Decode.field "flashcards" (Decode.list Flashcard.decoder))
