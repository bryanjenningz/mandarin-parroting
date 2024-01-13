module Video exposing (Video, VideoId)

import Subtitles exposing (Subtitles)
import VideoTime exposing (VideoTime)


type alias VideoId =
    String


type alias Video =
    { videoId : VideoId
    , title : String
    , duration : VideoTime
    , subtitles : Subtitles
    }
