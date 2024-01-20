module VideoTimeTests exposing (..)

import Expect
import Test exposing (Test, describe, test)
import VideoTime


videoTimeToStringTests : Test
videoTimeToStringTests =
    describe "VideoTime.toString"
        [ test "Stringifies 123.456 seconds to 00:02:03" <|
            \_ -> VideoTime.toString 123.456 |> Expect.equal "00:02:03"
        ]
