module Subtitles exposing (Subtitle, Subtitles, at, decoder, fromTranscript, next, prev, subtitlesParser, textParser, timeParser)

import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..))
import VideoTime exposing (VideoTime)


type alias Subtitles =
    List Subtitle


type alias Subtitle =
    { time : Float
    , text : String
    }


decoder : Decoder Subtitle
decoder =
    Decode.map2 (\time text -> { time = time, text = text })
        (Decode.field "time" Decode.float)
        (Decode.field "text" Decode.string)



-- PARSE FROM TRANSCRIPT


fromTranscript : String -> Result (List DeadEnd) (List Subtitle)
fromTranscript transcript =
    Parser.run subtitlesParser transcript


subtitlesParser : Parser (List Subtitle)
subtitlesParser =
    Parser.loop [] subtitlesParserHelper


subtitlesParserHelper : List Subtitle -> Parser (Step (List Subtitle) (List Subtitle))
subtitlesParserHelper revSubtitles =
    Parser.oneOf
        [ Parser.succeed (\subtitle -> Loop (subtitle :: revSubtitles))
            |= subtitleParser
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse revSubtitles))
        ]


subtitleParser : Parser Subtitle
subtitleParser =
    Parser.succeed (\time text -> { time = toFloat time, text = text })
        |. Parser.spaces
        |= timeParser
        |. Parser.spaces
        |= textParser
        |. Parser.spaces


textParser : Parser String
textParser =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.spaces
            |. Parser.chompUntilEndOr "\n"


timeParser : Parser Int
timeParser =
    Parser.succeed (\minutes seconds -> minutes * 60 + seconds)
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol "0"
                |= Parser.oneOf
                    [ Parser.int
                    , Parser.succeed 0
                    ]
            , Parser.int
            ]
        |. Parser.symbol ":"
        |. Parser.oneOf [ Parser.symbol "0", Parser.succeed () ]
        |= Parser.int



-- FIND SUBTITLES


at : VideoTime -> Subtitles -> Maybe Subtitle
at videoTime subtitles =
    case List.filter (\sub -> videoTime >= sub.time) subtitles |> List.last of
        Nothing ->
            List.head subtitles

        subtitle ->
            subtitle


next : VideoTime -> List Subtitle -> Maybe Subtitle
next videoTime subtitles =
    List.find (\sub -> videoTime < sub.time) subtitles


prev : VideoTime -> List Subtitle -> Maybe Subtitle
prev videoTime subtitles =
    let
        timeTolerance =
            0.8
    in
    case List.filter (\sub -> videoTime >= sub.time + timeTolerance) subtitles |> List.last of
        Nothing ->
            List.head subtitles

        subtitle ->
            subtitle
