module Subtitles exposing (fromTranscript, subtitlesParser, textParser, timeParser)

import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..))
import Video exposing (Subtitle)


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
