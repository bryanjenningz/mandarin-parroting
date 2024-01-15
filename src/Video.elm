module Video exposing (Video, VideoId, decoder, getById, viewCard, viewControls, viewSlider)

import Html exposing (Attribute, Html, button, div, h2, input, span, text)
import Html.Attributes as Attr exposing (attribute, class)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
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


decoder : Decoder Video
decoder =
    Decode.map4
        (\videoId title duration subtitles ->
            { videoId = videoId
            , title = title
            , duration = duration
            , subtitles = subtitles
            }
        )
        (Decode.field "videoId" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "duration" Decode.float)
        (Decode.field "subtitles" (Decode.list Subtitles.decoder))


getById : Maybe VideoId -> List Video -> Maybe Video
getById videoId videos =
    videoId |> Maybe.andThen (\id -> List.find (.videoId >> (==) id) videos)



-- VIEW


type alias VideoControlsProps msg =
    { videoIsPlaying : Bool
    , videoSpeed : Int
    , fastForward : msg
    , fastRewind : msg
    , setVideoSpeed : Int -> msg
    , playVideo : msg
    , pauseVideo : msg
    }


viewControls : VideoControlsProps msg -> Html msg
viewControls props =
    div [ class "w-full flex justify-between items-center gap-2" ]
        [ div [ class "basis-1 grow" ] []
        , div [ class "flex gap-2" ]
            [ button
                [ onClick props.fastRewind
                , class "bg-blue-600 rounded-lg w-12 h-12"
                ]
                [ labeledSymbol "Rewind" "<<" ]
            , playButton
                { videoIsPlaying = props.videoIsPlaying
                , playVideo = props.playVideo
                , pauseVideo = props.pauseVideo
                }
                [ class "bg-blue-600 rounded-lg w-12 h-12" ]
            , button
                [ onClick props.fastForward
                , class "bg-blue-600 rounded-lg w-12 h-12"
                ]
                [ labeledSymbol "Fast-forward" ">>" ]
            ]
        , div [ class "basis-1 grow" ]
            [ viewSpeed
                { videoSpeed = props.videoSpeed
                , setVideoSpeed = props.setVideoSpeed
                }
            ]
        ]


type alias ViewCardProps msg =
    { videoId : Maybe String
    , videoIsPlaying : Bool
    , startVideo : String -> msg
    , playVideo : msg
    , pauseVideo : msg
    }


viewCard : ViewCardProps msg -> Video -> Html msg
viewCard props video =
    div [ class "px-5 py-5 border border-white rounded-lg mx-auto w-full" ]
        [ div [ class "flex flex-col gap-4" ]
            [ h2 [ class "text-xl" ] [ text video.title ]
            , div [ class "flex justify-between" ]
                [ button
                    [ onClick (props.startVideo video.videoId)
                    , class "py-2 px-4 bg-blue-600 rounded-lg"
                    ]
                    [ text "Practice" ]
                , if props.videoId == Just video.videoId then
                    playButton
                        { videoIsPlaying = props.videoIsPlaying
                        , playVideo = props.playVideo
                        , pauseVideo = props.pauseVideo
                        }
                        [ class "bg-blue-600 rounded-lg w-12 h-12" ]

                  else
                    text ""
                ]
            ]
        ]


type alias ViewSliderProps msg =
    { videoTime : VideoTime
    , setVideoTime : VideoTime -> msg
    }


viewSlider : ViewSliderProps msg -> Video -> Html msg
viewSlider props video =
    div [ class "w-full" ]
        [ input
            [ Attr.type_ "range"
            , Attr.min "0"
            , Attr.max (String.fromFloat video.duration)
            , Attr.step "1"
            , Attr.value (String.fromFloat props.videoTime)
            , onInput
                (\inputText ->
                    inputText
                        |> String.toFloat
                        |> Maybe.map props.setVideoTime
                        |> Maybe.withDefault (props.setVideoTime 0)
                )
            , class "block w-full mx-auto"
            ]
            []
        ]



-- INTERNAL


type alias ViewSpeedProps msg =
    { videoSpeed : Int
    , setVideoSpeed : Int -> msg
    }


viewSpeed : ViewSpeedProps msg -> Html msg
viewSpeed props =
    div [ class "flex justify-end items-center gap-1" ]
        [ button
            [ onClick (props.setVideoSpeed (props.videoSpeed - 5))
            , class "bg-blue-600 rounded-lg w-6 h-6"
            ]
            [ labeledSymbol "Decrease video speed" "-" ]
        , div [ class "text-xs" ] [ text (String.fromInt props.videoSpeed ++ "%") ]
        , button
            [ onClick (props.setVideoSpeed (props.videoSpeed + 5))
            , class "bg-blue-600 rounded-lg w-6 h-6"
            ]
            [ labeledSymbol "Increase video speed" "+" ]
        ]


type alias PlayButtonProps msg =
    { videoIsPlaying : Bool
    , playVideo : msg
    , pauseVideo : msg
    }


playButton : PlayButtonProps msg -> List (Attribute msg) -> Html msg
playButton props attributes =
    if props.videoIsPlaying then
        button (attributes ++ [ onClick props.pauseVideo ])
            [ labeledSymbol "Pause" "||" ]

    else
        button (attributes ++ [ onClick props.playVideo ])
            [ labeledSymbol "Play" "▶" ]


labeledSymbol : String -> String -> Html msg
labeledSymbol label symbol =
    span []
        [ span [ attribute "aria-hidden" "true" ] [ text symbol ]
        , span [ class "sr-only" ] [ text label ]
        ]
