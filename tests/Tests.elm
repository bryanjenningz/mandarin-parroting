module Tests exposing (..)

import Expect
import Subtitle
import Test exposing (Test, describe, test)


subtitleAtTests : Test
subtitleAtTests =
    describe "Subtitle.at"
        [ test "Returns nothing if there are no subtitles" <|
            \_ ->
                Subtitle.at 5 [] |> Expect.equal Nothing
        , test "Returns the first subtitle if the time is before all subtitles 1" <|
            \_ ->
                Subtitle.at 5
                    [ { text = "b", time = 123 } ]
                    |> Expect.equal (Just { text = "b", time = 123 })
        , test "Returns the first subtitle if the time is before all subtitles 2" <|
            \_ ->
                Subtitle.at 5
                    [ { text = "b", time = 123 }
                    , { text = "b2", time = 456 }
                    ]
                    |> Expect.equal (Just { text = "b", time = 123 })
        , test "Returns the first subtitle that the time is at or above 1" <|
            \_ ->
                Subtitle.at 123
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b1", time = 123 })
        , test "Returns the first subtitle that the time is at or above 2" <|
            \_ ->
                Subtitle.at 124
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b1", time = 123 })
        , test "Returns the first subtitle that the time is at or above 3" <|
            \_ ->
                Subtitle.at 456
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b2", time = 456 })
        , test "Returns the first subtitle that the time is at or above 4" <|
            \_ ->
                Subtitle.at 788
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b2", time = 456 })
        , test "Returns the first subtitle that the time is at or above 5" <|
            \_ ->
                Subtitle.at 789
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b3", time = 789 })
        , test "Returns the first subtitle that the time is at or above 6" <|
            \_ ->
                Subtitle.at 9999
                    [ { text = "b1", time = 123 }
                    , { text = "b2", time = 456 }
                    , { text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { text = "b3", time = 789 })
        ]


subtitleFromTranscriptTests : Test
subtitleFromTranscriptTests =
    describe "Subtitle.fromTranscript"
        [ test "Returns a single subtitle for a transcript with a single subtitle" <|
            \_ ->
                Subtitle.fromTranscript """WEBVTT
Kind: captions
Language: zh-TW

00:00:12.200 --> 00:00:14.933
大家好 我是阿倫

"""
                    |> Expect.equal (Ok [ { text = "大家好 我是阿倫", time = 12200 } ])
        , test "Returns 3 subtitles for a transcript with 3 subtitles" <|
            \_ ->
                Subtitle.fromTranscript """WEBVTT
Kind: captions
Language: zh

00:00:00.000 --> 00:00:02.366
大家好 欢迎来到公子时评节目

00:00:02.366 --> 00:00:03.400
我是公子沈

00:00:03.400 --> 00:00:04.166
在此之前

"""
                    |> Expect.equal
                        (Ok
                            [ { text = "大家好 欢迎来到公子时评节目", time = 0 }
                            , { text = "我是公子沈", time = 2366 }
                            , { text = "在此之前", time = 3400 }
                            ]
                        )
        , test "Returns 4 subtitles around the 1 minute mark for a transcript with 4 subtitles" <|
            \_ ->
                Subtitle.fromTranscript """WEBVTT
Kind: captions
Language: zh-Hans

00:00:56.832 --> 00:01:03.232
现场有25万人参加规模非常大

00:01:03.232 --> 00:01:07.072
赖清德在晚会上说全世界都在关注

00:01:07.072 --> 00:01:10.400
台湾人民会做出怎样的选择

00:01:10.400 --> 00:01:14.496
民进党坚持民主价值和中国偏好的

"""
                    |> Expect.equal
                        (Ok
                            [ { text = "现场有25万人参加规模非常大", time = 56832 }
                            , { text = "赖清德在晚会上说全世界都在关注", time = 63232 }
                            , { text = "台湾人民会做出怎样的选择", time = 67072 }
                            , { text = "民进党坚持民主价值和中国偏好的", time = 70400 }
                            ]
                        )
        ]
