module Tests exposing (..)

import Expect
import NewVideo
import Parser
import Test exposing (Test, describe, test)
import Video


subtitleAtTests : Test
subtitleAtTests =
    describe "Video.subtitleAt"
        [ test "Returns nothing if there are no subtitles" <|
            \_ ->
                Video.subtitleAt 5 [] |> Expect.equal Nothing
        , test "Returns the first subtitle if the time is before all subtitles 1" <|
            \_ ->
                Video.subtitleAt 5
                    [ { text = "b", time = 123 } ]
                    |> Expect.equal (Just { text = "b", time = 123 })
        , test "Returns the first subtitle if the time is before all subtitles 2" <|
            \_ ->
                Video.subtitleAt 5
                    [ { text = "b", time = 123 }
                    , { text = "b2", time = 456 }
                    ]
                    |> Expect.equal (Just { text = "b", time = 123 })
        , test "Returns the first subtitle that the time is at or above 1" <|
            \_ ->
                Video.subtitleAt 123
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b1", time = 123 })
        , test "Returns the first subtitle that the time is at or above 2" <|
            \_ ->
                Video.subtitleAt 124
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b1", time = 123 })
        , test "Returns the first subtitle that the time is at or above 3" <|
            \_ ->
                Video.subtitleAt 456
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b2", time = 456 })
        , test "Returns the first subtitle that the time is at or above 4" <|
            \_ ->
                Video.subtitleAt 788
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b2", time = 456 })
        , test "Returns the first subtitle that the time is at or above 5" <|
            \_ ->
                Video.subtitleAt 789
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b3", time = 789 })
        , test "Returns the first subtitle that the time is at or above 6" <|
            \_ ->
                Video.subtitleAt 9999
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b3", time = 789 })
        ]


transcriptToSubtitlesTests : Test
transcriptToSubtitlesTests =
    describe "NewVideo.transcriptToSubtitles"
        [ test "Returns an empty list for an empty transcript" <|
            \_ ->
                NewVideo.transcriptToSubtitles ""
                    |> Expect.equal (Ok [])
        , test "Returns a single entry for a single line transcript" <|
            \_ ->
                NewVideo.transcriptToSubtitles "\n0:01\nHello\n"
                    |> Expect.equal (Ok [ { text = "Hello", time = 1 } ])
        , test "Returns 2 entries for a 2-line transcript" <|
            \_ ->
                NewVideo.transcriptToSubtitles "\n0:01\nHello\n1:01\nHi"
                    |> Expect.equal
                        (Ok
                            [ { text = "Hello", time = 1 }
                            , { text = "Hi", time = 61 }
                            ]
                        )
        , test "Returns 3 entries for a 3-line transcript" <|
            \_ ->
                NewVideo.transcriptToSubtitles
                    """0:00
各位同学大家好 我是李永乐老师
0:02
最近全国很多地方降温幅度非常大
0:05
很多小伙伴都在家里面"""
                    |> Expect.equal
                        (Ok
                            [ { text = "各位同学大家好 我是李永乐老师", time = 0 }
                            , { text = "最近全国很多地方降温幅度非常大", time = 2 }
                            , { text = "很多小伙伴都在家里面", time = 5 }
                            ]
                        )
        ]


timeParserTests : Test
timeParserTests =
    describe "NewVideo.timeParser"
        [ test "Parses 0:12 as 12" <|
            \_ ->
                Parser.run NewVideo.timeParser "0:12"
                    |> Expect.equal (Ok 12)
        , test "Parses 1:23 as 83" <|
            \_ ->
                Parser.run NewVideo.timeParser "1:23"
                    |> Expect.equal (Ok 83)
        , test "Parses 0:05 as 5" <|
            \_ ->
                Parser.run NewVideo.timeParser "0:05"
                    |> Expect.equal (Ok 5)
        , test "Parses 0:59 as 59" <|
            \_ ->
                Parser.run NewVideo.timeParser "0:59"
                    |> Expect.equal (Ok 59)
        , test "Parses 10:59 as 659" <|
            \_ ->
                Parser.run NewVideo.timeParser "10:59"
                    |> Expect.equal (Ok 659)
        , test "Parses 10:00 as 600" <|
            \_ ->
                Parser.run NewVideo.timeParser "10:00"
                    |> Expect.equal (Ok 600)
        , test "Parses 1:01 as 61" <|
            \_ ->
                Parser.run NewVideo.timeParser "1:01"
                    |> Expect.equal (Ok 61)
        ]
