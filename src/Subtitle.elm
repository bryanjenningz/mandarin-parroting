module Subtitle exposing (Subtitle, at, decoder, encoder, fromTranscript, jumpTo, next, prev, view)

import Browser.Dom as Dom
import Dictionary
import Flashcard exposing (Flashcard)
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as List
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..))
import Task
import VideoTime exposing (VideoTime)


type alias Subtitle =
    { time : Float
    , text : String
    }


decoder : Decoder Subtitle
decoder =
    Decode.map2 (\time text -> { time = time, text = text })
        (Decode.field "time" Decode.float)
        (Decode.field "text" Decode.string)


encoder : Subtitle -> Encode.Value
encoder subtitle =
    Encode.object
        [ ( "time", Encode.float subtitle.time )
        , ( "text", Encode.string subtitle.text )
        ]



-- PARSE FROM TRANSCRIPT


fromTranscript : String -> Result (List DeadEnd) (List Subtitle)
fromTranscript transcript =
    Parser.run subtitlesParser transcript



-- FIND SUBTITLES


at : VideoTime -> List Subtitle -> Maybe Subtitle
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



-- VIEW


type alias ViewSubtitlesProps msg =
    { currentSubtitle : Subtitle
    , subtitles : List Subtitle
    , setVideoTime : Float -> msg
    , dictionary : Dictionary.Model
    , dictionaryLookup : Maybe ( Subtitle, Int )
    , setDictionaryLookup : Maybe ( Subtitle, Int ) -> msg
    , flashcards : List Flashcard
    , saveFlashcard : Flashcard -> msg
    , deleteFlashcard : Flashcard -> msg
    }


view : ViewSubtitlesProps msg -> Html msg
view props =
    div [ Attr.id subtitlesContainerId, class "overflow-y-scroll" ]
        [ div []
            (props.subtitles
                |> List.map
                    (\subtitle ->
                        div
                            [ class "text-2xl flex justify-between items-center"
                            , classList
                                [ ( "text-blue-400", subtitle == props.currentSubtitle ) ]
                            , Attr.id (subtitleId subtitle)
                            ]
                            [ div []
                                (List.indexedMap
                                    (\i char ->
                                        let
                                            selected =
                                                props.dictionaryLookup
                                                    |> Maybe.map (\( sub, index ) -> sub == subtitle && index == i)
                                                    |> Maybe.withDefault False
                                        in
                                        span
                                            [ onClick (props.setDictionaryLookup (Just ( subtitle, i )))
                                            , class "relative"
                                            , classList [ ( "bg-blue-600 text-white", selected ) ]
                                            ]
                                            [ text char
                                            , if selected then
                                                div [ class "absolute top-100 min-w-64 z-20 bg-black text-white p-4 rounded-lg border border-white" ]
                                                    [ Dictionary.view
                                                        { searchText = String.dropLeft i subtitle.text
                                                        , dictionary = props.dictionary
                                                        , flashcards = props.flashcards
                                                        , saveFlashcard = props.saveFlashcard
                                                        , deleteFlashcard = props.deleteFlashcard
                                                        }
                                                    ]

                                              else
                                                text ""
                                            ]
                                    )
                                    (String.split "" subtitle.text)
                                )
                            , button
                                [ onClick (props.setVideoTime subtitle.time)
                                , class "bg-blue-600 text-white rounded-lg w-6 h-6 text-sm"
                                ]
                                [ text "▶" ]
                            ]
                    )
            )
        , case props.dictionaryLookup of
            Nothing ->
                text ""

            Just _ ->
                div
                    [ class "absolute inset-0 z-10"
                    , onClick (props.setDictionaryLookup Nothing)
                    ]
                    []
        ]



-- JUMP TO SUBTITLE


type alias JumpToSubtitleProps msg =
    { subtitle : Subtitle
    , noop : msg
    }


jumpTo : JumpToSubtitleProps msg -> Cmd msg
jumpTo props =
    let
        padding =
            500
    in
    Dom.getViewportOf subtitlesContainerId
        |> Task.andThen
            (\{ viewport } ->
                Dom.getElement (subtitleId props.subtitle)
                    |> Task.map (\{ element } -> ( viewport, element ))
            )
        |> Task.andThen
            (\( viewport, element ) ->
                Dom.setViewportOf subtitlesContainerId
                    0
                    (viewport.y + element.y - padding)
            )
        |> Task.attempt (\_ -> props.noop)



-- INTERNAL


subtitlesParser : Parser (List Subtitle)
subtitlesParser =
    Parser.succeed identity
        |. Parser.token "WEBVTT"
        |. Parser.spaces
        |. Parser.token "Kind: captions"
        |. Parser.spaces
        |. Parser.oneOf
            [ Parser.token "Language: zh-Hans"
            , Parser.token "Language: zh-TW"
            , Parser.token "Language: zh"
            ]
        |. Parser.spaces
        |= Parser.loop [] subtitlesParserHelper
        |. Parser.spaces
        |. Parser.end


subtitlesParserHelper : List Subtitle -> Parser (Step (List Subtitle) (List Subtitle))
subtitlesParserHelper revSubtitles =
    Parser.oneOf
        [ subtitleParser
            |> Parser.map (\subtitle -> Loop (subtitle :: revSubtitles))
        , Parser.succeed (Done (List.reverse revSubtitles))
        ]


subtitleParser : Parser Subtitle
subtitleParser =
    Parser.succeed (\time text -> { time = toFloat time, text = text })
        |= timeParser
        |. Parser.spaces
        |. Parser.symbol "-->"
        |. Parser.spaces
        |. timeParser
        |. Parser.spaces
        |= textParser
        |. Parser.spaces


timeParser : Parser Int
timeParser =
    Parser.succeed
        (\hours minutes seconds milliseconds ->
            (hours * 60 * 60 * 1000)
                + (minutes * 60 * 1000)
                + (seconds * 1000)
                + milliseconds
        )
        |= timeSegmentParser
        |. Parser.symbol ":"
        |= timeSegmentParser
        |. Parser.symbol ":"
        |= timeSegmentParser
        |. Parser.symbol "."
        |= timeSegmentMillisecondsParser


timeSegmentParser : Parser Int
timeSegmentParser =
    Parser.succeed (\first second -> first * 10 + second)
        |= digitParser
        |= digitParser


timeSegmentMillisecondsParser : Parser Int
timeSegmentMillisecondsParser =
    Parser.succeed
        (\first second third -> first * 100 + second * 10 + third)
        |= digitParser
        |= digitParser
        |= digitParser


digitParser : Parser Int
digitParser =
    Parser.getChompedString (Parser.chompIf Char.isDigit)
        |> Parser.andThen
            (\ch ->
                case String.toInt ch of
                    Nothing ->
                        Parser.problem "Expected a digit"

                    Just digit ->
                        Parser.succeed digit
            )


textParser : Parser String
textParser =
    Parser.succeed identity
        |= Parser.getChompedString (Parser.chompWhile (\ch -> ch /= '\n'))


subtitlesContainerId : String
subtitlesContainerId =
    "subtitles-container"


subtitleId : Subtitle -> String
subtitleId subtitle =
    "sub" ++ String.fromFloat subtitle.time
