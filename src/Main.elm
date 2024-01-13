port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, Html, button, div, h2, input, span, text)
import Html.Attributes as Attr exposing (attribute, class, classList)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import NewVideo exposing (NewVideo)
import Subtitles exposing (Subtitle, Subtitles)
import Task
import Video exposing (Video, VideoId)
import VideoTime exposing (VideoTime)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { tab : Tab
    , videoId : Maybe VideoId
    , videoIsPlaying : Bool
    , videoTime : VideoTime
    , videos : List Video
    , newVideo : NewVideo
    , newVideoError : Maybe NewVideo.Error
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tab = SelectVideoTab
      , videoId = Nothing
      , videoIsPlaying = False
      , videoTime = 0
      , videos = []
      , newVideo = NewVideo.empty
      , newVideoError = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | TabClicked Tab
    | StartVideo VideoId
    | PlayVideo
    | PauseVideo
    | FastForward
    | FastRewind
    | GetVideoTime VideoTime
    | SetVideoTime VideoTime
    | JumpToSubtitle Subtitle
    | SetNewVideoId VideoId
    | SetNewVideoTranscript String
    | SubmitNewVideo
    | AddVideo Video


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TabClicked tab ->
            ( { model | tab = tab }, Cmd.none )

        StartVideo videoId ->
            ( { model
                | tab = PlayVideoTab
                , videoId = Just videoId
                , videoIsPlaying = True
              }
            , if Just videoId == model.videoId then
                playVideo ()

              else
                startVideo videoId
            )

        PlayVideo ->
            ( { model | videoIsPlaying = True }, playVideo () )

        PauseVideo ->
            ( { model | videoIsPlaying = False }, pauseVideo () )

        FastForward ->
            let
                maybeNextSubtitleTime : Maybe VideoTime
                maybeNextSubtitleTime =
                    getVideo model.videoId model.videos
                        |> Maybe.map .subtitles
                        |> Maybe.andThen (Subtitles.next model.videoTime)
                        |> Maybe.map .time
            in
            case maybeNextSubtitleTime of
                Nothing ->
                    ( model, Cmd.none )

                Just videoTime ->
                    ( model, setVideoTime videoTime )

        FastRewind ->
            let
                maybePrevSubtitleTime : Maybe VideoTime
                maybePrevSubtitleTime =
                    getVideo model.videoId model.videos
                        |> Maybe.map .subtitles
                        |> Maybe.andThen (Subtitles.prev model.videoTime)
                        |> Maybe.map .time
            in
            case maybePrevSubtitleTime of
                Nothing ->
                    ( model, Cmd.none )

                Just videoTime ->
                    ( model, setVideoTime videoTime )

        GetVideoTime videoTime ->
            ( { model | videoTime = videoTime }, Cmd.none )

        SetVideoTime videoTime ->
            ( model, setVideoTime videoTime )

        JumpToSubtitle subtitle ->
            ( model, jumpToSubtitle subtitle )

        SetNewVideoId newVideoId ->
            ( { model | newVideo = NewVideo.setVideoId newVideoId model.newVideo }
            , Cmd.none
            )

        SetNewVideoTranscript newVideoTranscript ->
            ( { model | newVideo = NewVideo.setTranscript newVideoTranscript model.newVideo }
            , Cmd.none
            )

        SubmitNewVideo ->
            case NewVideo.validate model.newVideo of
                Ok validNewVideo ->
                    ( { model | newVideoError = Nothing }
                    , submitNewVideo (NewVideo.encode validNewVideo)
                    )

                Err error ->
                    ( { model | newVideoError = Just error }, Cmd.none )

        AddVideo video ->
            ( { model | videos = model.videos ++ [ video ] }, Cmd.none )


getVideo : Maybe VideoId -> List Video -> Maybe Video
getVideo videoId videos =
    videoId |> Maybe.andThen (\id -> List.find (.videoId >> (==) id) videos)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center" ]
        [ div [ class "fixed w-full" ] [ viewTabs model ]
        , div [ class "w-full max-w-2xl" ]
            [ div [ class "pt-24 px-3" ]
                [ case model.tab of
                    SelectVideoTab ->
                        viewSelectVideoTab model

                    PlayVideoTab ->
                        viewPlayVideoTab model
                ]
            ]
        ]


type Tab
    = SelectVideoTab
    | PlayVideoTab


tabs : List Tab
tabs =
    [ SelectVideoTab
    , PlayVideoTab
    ]


viewTabs : Model -> Html Msg
viewTabs model =
    div [ class "h-16 flex text-xl bg-black" ]
        (List.map (viewTab model) tabs)


viewTab : Model -> Tab -> Html Msg
viewTab model tab =
    button
        [ classList
            [ ( "text-blue-400 border-blue-400", model.tab == tab )
            , ( "text-white border-black", model.tab /= tab )
            ]
        , class "grow h-full flex justify-center items-center cursor-pointer border-b-2"
        , onClick (TabClicked tab)
        ]
        [ text <|
            case tab of
                SelectVideoTab ->
                    "Videos"

                PlayVideoTab ->
                    "Practice"
        ]


viewSelectVideoTab : Model -> Html Msg
viewSelectVideoTab model =
    div [ class "flex flex-col items-center gap-4" ]
        [ NewVideo.view
            { setNewVideoId = SetNewVideoId
            , setNewVideoTranscript = SetNewVideoTranscript
            , submitNewVideo = SubmitNewVideo
            }
        , div [ class "flex flex-col gap-4" ]
            (List.map (viewVideoCard model) model.videos)
        ]


viewPlayVideoTab : Model -> Html Msg
viewPlayVideoTab model =
    case getVideo model.videoId model.videos of
        Nothing ->
            div [ class "flex flex-col items-center" ]
                [ div [ class "w-full max-w-2xl flex flex-col items-center gap-4" ]
                    [ h2 [ class "text-2xl" ]
                        [ text <|
                            if List.isEmpty model.videos then
                                "You haven't added any videos yet."

                            else
                                "You haven't selected a video."
                        ]
                    , button
                        [ onClick (TabClicked SelectVideoTab)
                        , class "w-full py-2 px-4 bg-blue-600 rounded-lg"
                        ]
                        [ text <|
                            if List.isEmpty model.videos then
                                "Add a video"

                            else
                                "Select a video"
                        ]
                    ]
                ]

        Just video ->
            let
                currentSubtitle =
                    Subtitles.at model.videoTime video.subtitles
            in
            div [ class "flex flex-col items-center gap-2 h-full" ]
                [ div [ class "text-xl text-center" ] [ text video.title ]
                , viewVideoSlider model.videoTime video
                , div []
                    [ text
                        (VideoTime.toString model.videoTime
                            ++ " / "
                            ++ VideoTime.toString video.duration
                        )
                    ]
                , case currentSubtitle of
                    Nothing ->
                        text ""

                    Just subtitle ->
                        button [ onClick (JumpToSubtitle subtitle) ]
                            [ text subtitle.text ]
                , viewVideoControls model
                , case currentSubtitle of
                    Nothing ->
                        text ""

                    Just subtitle ->
                        viewSubtitles subtitle video.subtitles
                ]


viewVideoSlider : VideoTime -> Video -> Html Msg
viewVideoSlider videoTime video =
    div [ class "w-full" ]
        [ input
            [ Attr.type_ "range"
            , Attr.min "0"
            , Attr.max (String.fromFloat video.duration)
            , Attr.step "1"
            , Attr.value (String.fromFloat videoTime)
            , onInput
                (\inputText ->
                    inputText
                        |> String.toFloat
                        |> Maybe.map SetVideoTime
                        |> Maybe.withDefault (SetVideoTime 0)
                )
            , class "block w-full md:w-3/4 lg:w-1/2 mx-auto"
            ]
            []
        ]


viewVideoControls : Model -> Html Msg
viewVideoControls model =
    div [ class "flex gap-2" ]
        [ button
            [ onClick FastRewind
            , class "bg-cyan-500 w-12 h-12 hover:bg-cyan-600"
            ]
            [ labeledSymbol "Rewind" "<<" ]
        , playButton model [ class "bg-cyan-500 w-12 h-12 hover:bg-cyan-600" ]
        , button
            [ onClick FastForward
            , class "bg-cyan-500 w-12 h-12 hover:bg-cyan-600"
            ]
            [ labeledSymbol "Fast-forward" ">>" ]
        ]


viewSubtitles : Subtitle -> Subtitles -> Html Msg
viewSubtitles currentSubtitle subtitles =
    div [ Attr.id subtitlesContainerId, class "overflow-y-scroll h-1/2 md:h-3/5" ]
        (subtitles
            |> List.map
                (\subtitle ->
                    div
                        [ class "text-center"
                        , classList
                            [ ( "text-cyan-300", subtitle == currentSubtitle ) ]
                        , onClick (SetVideoTime subtitle.time)
                        , Attr.id (subtitleId subtitle)
                        ]
                        [ text subtitle.text ]
                )
        )


subtitlesContainerId : String
subtitlesContainerId =
    "subtitles-container"


subtitleId : Subtitle -> String
subtitleId subtitle =
    "sub" ++ String.fromFloat subtitle.time


jumpToSubtitle : Subtitle -> Cmd Msg
jumpToSubtitle subtitle =
    let
        padding =
            500
    in
    Dom.getViewportOf subtitlesContainerId
        |> Task.andThen
            (\{ viewport } ->
                Dom.getElement (subtitleId subtitle)
                    |> Task.map (\{ element } -> ( viewport, element ))
            )
        |> Task.andThen
            (\( viewport, element ) ->
                Dom.setViewportOf subtitlesContainerId
                    0
                    (viewport.y + element.y - padding)
            )
        |> Task.attempt (\_ -> NoOp)


labeledSymbol : String -> String -> Html msg
labeledSymbol label symbol =
    span []
        [ span [ attribute "aria-hidden" "true" ] [ text symbol ]
        , span [ class "sr-only" ] [ text label ]
        ]


viewVideoCard : Model -> Video -> Html Msg
viewVideoCard model video =
    div [ class "px-5 py-5 border border-white rounded-lg mx-auto w-full" ]
        [ div [ class "flex flex-col gap-4" ]
            [ h2 [ class "text-xl" ] [ text video.title ]
            , div [ class "flex justify-between" ]
                [ button
                    [ onClick (StartVideo video.videoId)
                    , class "py-2 px-4 bg-blue-600 rounded-lg"
                    ]
                    [ text "Practice" ]
                , if model.videoId == Just video.videoId then
                    playButton model [ class "bg-blue-600 rounded-lg w-12 h-12" ]

                  else
                    text ""
                ]
            ]
        ]


playButton : Model -> List (Attribute Msg) -> Html Msg
playButton model attributes =
    if model.videoIsPlaying then
        button (attributes ++ [ onClick PauseVideo ])
            [ labeledSymbol "Pause" "||" ]

    else
        button (attributes ++ [ onClick PlayVideo ])
            [ labeledSymbol "Play" "▶" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ getVideoTime GetVideoTime
        , addVideo AddVideo
        ]


port startVideo : VideoId -> Cmd msg


port playVideo : () -> Cmd msg


port pauseVideo : () -> Cmd msg


port getVideoTime : (VideoTime -> msg) -> Sub msg


port setVideoTime : VideoTime -> Cmd msg


port submitNewVideo : { videoId : String, subtitles : List Subtitle } -> Cmd msg


port addVideo : (Video -> msg) -> Sub msg
