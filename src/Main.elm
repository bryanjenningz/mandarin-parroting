port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, button, div, h2, text)
import Html.Attributes as Attr exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder, Value)
import NewVideo exposing (NewVideo)
import Subtitles exposing (Subtitle, Subtitles)
import Task
import Video exposing (Video, VideoId)
import VideoTime exposing (VideoTime)


main : Program Value Model Msg
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
    , videoSpeed : Int
    , videos : List Video
    , newVideo : NewVideo
    , newVideoError : Maybe NewVideo.Error
    }


type alias Flags =
    { videoId : Maybe VideoId
    , videoSpeed : Int
    , videos : List Video
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map3
        (\videoId videoSpeed videos ->
            { videoId = videoId, videoSpeed = videoSpeed, videos = videos }
        )
        (Decode.field "videoId" (Decode.maybe Decode.string))
        (Decode.field "videoSpeed" Decode.int)
        (Decode.field "videos" (Decode.list Video.decoder))


flagsDefault : Flags
flagsDefault =
    { videoId = Nothing
    , videoSpeed = 100
    , videos = []
    }


init : Value -> ( Model, Cmd Msg )
init value =
    let
        flags =
            value
                |> Decode.decodeValue Decode.string
                |> Result.andThen (Decode.decodeString flagsDecoder)
                |> Result.withDefault flagsDefault
    in
    ( { tab = SelectVideoTab
      , videoId = flags.videoId
      , videoIsPlaying = False
      , videoTime = 0
      , videoSpeed = flags.videoSpeed
      , videos = flags.videos
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
    | SetVideoSpeed Int
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
            let
                newModel =
                    { model
                        | tab = PlayVideoTab
                        , videoId = Just videoId
                        , videoIsPlaying = True
                    }
            in
            ( newModel
            , Cmd.batch
                [ if Just videoId == model.videoId then
                    playVideo ()

                  else
                    startVideo videoId
                , saveFlags
                    { videoId = newModel.videoId
                    , videoSpeed = newModel.videoSpeed
                    , videos = newModel.videos
                    }
                ]
            )

        PlayVideo ->
            ( { model | videoIsPlaying = True }, playVideo () )

        PauseVideo ->
            ( { model | videoIsPlaying = False }, pauseVideo () )

        FastForward ->
            let
                maybeNextSubtitleTime : Maybe VideoTime
                maybeNextSubtitleTime =
                    Video.getById model.videoId model.videos
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
                    Video.getById model.videoId model.videos
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

        SetVideoSpeed videoSpeed ->
            let
                newModel =
                    { model | videoSpeed = clamp 50 200 videoSpeed }
            in
            ( newModel
            , Cmd.batch
                [ setVideoSpeed videoSpeed
                , saveFlags
                    { videoId = newModel.videoId
                    , videoSpeed = newModel.videoSpeed
                    , videos = newModel.videos
                    }
                ]
            )

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
            let
                newModel =
                    { model | videos = model.videos ++ [ video ] }
            in
            ( newModel
            , saveFlags
                { videoId = newModel.videoId
                , videoSpeed = newModel.videoSpeed
                , videos = newModel.videos
                }
            )



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
            , newVideoError = model.newVideoError
            }
        , div [ class "flex flex-col gap-4" ]
            (List.map
                (Video.viewCard
                    { pauseVideo = PauseVideo
                    , playVideo = PlayVideo
                    , startVideo = StartVideo
                    , videoId = model.videoId
                    , videoIsPlaying = model.videoIsPlaying
                    }
                )
                model.videos
            )
        ]


viewPlayVideoTab : Model -> Html Msg
viewPlayVideoTab model =
    case Video.getById model.videoId model.videos of
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
            div [ class "flex flex-col items-center gap-2 h-[80vh]" ]
                [ div [ class "text-xl text-center" ] [ text video.title ]
                , Video.viewSlider
                    { videoTime = model.videoTime
                    , setVideoTime = SetVideoTime
                    }
                    video
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
                        button [ onClick (JumpToSubtitle subtitle), class "text-xl" ]
                            [ text subtitle.text ]
                , Video.viewControls
                    { videoIsPlaying = model.videoIsPlaying
                    , videoSpeed = model.videoSpeed
                    , fastForward = FastForward
                    , fastRewind = FastRewind
                    , setVideoSpeed = SetVideoSpeed
                    , playVideo = PlayVideo
                    , pauseVideo = PauseVideo
                    }
                , case currentSubtitle of
                    Nothing ->
                        text ""

                    Just subtitle ->
                        viewSubtitles subtitle video.subtitles
                ]


viewSubtitles : Subtitle -> Subtitles -> Html Msg
viewSubtitles currentSubtitle subtitles =
    div [ Attr.id subtitlesContainerId, class "overflow-y-scroll" ]
        (subtitles
            |> List.map
                (\subtitle ->
                    div
                        [ class "text-center text-2xl"
                        , classList
                            [ ( "text-blue-400", subtitle == currentSubtitle ) ]
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


port setVideoSpeed : Int -> Cmd msg


port submitNewVideo : { videoId : String, subtitles : List Subtitle } -> Cmd msg


port addVideo : (Video -> msg) -> Sub msg


port saveFlags : Flags -> Cmd msg
