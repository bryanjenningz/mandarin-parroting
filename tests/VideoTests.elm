module VideoTests exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Video


videoGetByIdTests : Test
videoGetByIdTests =
    describe "Video.getById"
        [ test "Returns Nothing if you pass in Nothing for the videoId" <|
            \_ -> Video.getById Nothing [] |> Expect.equal Nothing
        , test "Returns Nothing if you pass in an empty list of videos" <|
            \_ -> Video.getById (Just "abc") [] |> Expect.equal Nothing
        , test "Returns the video with the video ID you passed in" <|
            \_ ->
                Video.getById (Just "videoId2")
                    [ { videoId = "videoId1", title = "title1", duration = 0.01, subtitles = [] }
                    , { videoId = "videoId2", title = "title2", duration = 0.02, subtitles = [] }
                    , { videoId = "videoId3", title = "title3", duration = 0.03, subtitles = [] }
                    ]
                    |> Expect.equal (Just { videoId = "videoId2", title = "title2", duration = 0.02, subtitles = [] })
        ]
