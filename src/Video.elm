module Video exposing (Subtitle, Video, VideoId, nextSubtitle, prevSubtitle, subtitleAt)

import List.Extra as List
import VideoTime exposing (VideoTime)


type alias VideoId =
    String


type alias Video =
    { videoId : VideoId
    , title : String
    , duration : VideoTime
    , subtitles : List Subtitle
    }


type alias Subtitle =
    { videoId : VideoId
    , text : String
    , time : VideoTime
    }


subtitleAt : VideoTime -> List Subtitle -> Maybe Subtitle
subtitleAt videoTime subtitles =
    case List.filter (\sub -> videoTime >= sub.time) subtitles |> List.last of
        Nothing ->
            List.head subtitles

        subtitle ->
            subtitle


nextSubtitle : VideoTime -> List Subtitle -> Maybe Subtitle
nextSubtitle videoTime subtitles =
    List.find (\sub -> videoTime < sub.time) subtitles


prevSubtitle : VideoTime -> List Subtitle -> Maybe Subtitle
prevSubtitle videoTime subtitles =
    let
        timeTolerance =
            0.8
    in
    case List.filter (\sub -> videoTime >= sub.time + timeTolerance) subtitles |> List.last of
        Nothing ->
            List.head subtitles

        subtitle ->
            subtitle
