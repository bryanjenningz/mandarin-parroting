module Flags exposing (Flags, decode)

import Json.Decode as Decode exposing (Decoder, Value)
import Video exposing (Video, VideoId)


type alias Flags =
    { videoId : Maybe VideoId
    , videoSpeed : Int
    , videos : List Video
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
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map3
        (\videoId videoSpeed videos ->
            { videoId = videoId, videoSpeed = videoSpeed, videos = videos }
        )
        (Decode.field "videoId" (Decode.maybe Decode.string))
        (Decode.field "videoSpeed" Decode.int)
        (Decode.field "videos" (Decode.list Video.decoder))
