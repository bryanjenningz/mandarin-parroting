port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, Html)
import Html.Attributes as Attr exposing (attribute, class, classList)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Json
import List.Extra as List
import Task
import Video exposing (Subtitle, Video, VideoId, VideoTime, decodeVideo, getNextSubtitle, getPrevSubtitle, getSubtitleAt)


backendUrlRoot : String
backendUrlRoot =
    "https://raw.githubusercontent.com/bryanjenningz/listening-practice-web-app/main/static/"


type alias TabData =
    { text : String
    , icon : String
    , content : Model -> Html Msg
    }


findTabId : TabId
findTabId =
    0


listenTabId : TabId
listenTabId =
    1


tabs : List TabData
tabs =
    [ { text = "Find"
      , icon = "search"
      , content = viewFindTab
      }
    , { text = "Listen"
      , icon = "headphones"
      , content = viewListenTab
      }
    , { text = "Review"
      , icon = "grading"
      , content = viewReviewTab
      }
    ]


type alias Model =
    { tabId : TabId
    , videoId : Maybe VideoId
    , videoIsPlaying : Bool
    , videoTime : VideoTime
    , videos : List Video
    , recordings : List Recording
    }


type alias TabId =
    Int


type alias Recording =
    { videoId : VideoId
    , time : Float
    , text : String
    }


decodeRecordings : Json.Decoder (List Recording)
decodeRecordings =
    Json.list <|
        Json.map3 Recording
            (Json.field "videoId" Json.string)
            (Json.field "time" Json.float)
            (Json.field "text" Json.string)


getVideo : Maybe VideoId -> List Video -> Maybe Video
getVideo videoId videos =
    videoId |> Maybe.andThen (\id -> List.find (.id >> (==) id) videos)


fetchVideo : String -> Cmd Msg
fetchVideo videoId =
    Http.get
        { url = backendUrlRoot ++ videoId ++ ".json"
        , expect = Http.expectJson GotVideo (decodeVideo videoId)
        }


videoIds : List String
videoIds =
    [ "bmgZsgcPd_8"
    , "gx874_psuUk"
    , "RUkjrZZolSo"
    ]


init : Json.Value -> ( Model, Cmd Msg )
init recordingsJson =
    ( { tabId = 0
      , videoId = Nothing
      , videoIsPlaying = False
      , videoTime = 0
      , videos = []
      , recordings =
            recordingsJson
                |> Json.decodeValue Json.string
                |> Result.andThen (Json.decodeString decodeRecordings)
                |> Result.withDefault []
      }
    , Cmd.batch (List.map fetchVideo videoIds)
    )


type Msg
    = NoOp
    | TabClicked TabId
    | ListenToVideo VideoId
    | PlayVideo
    | PauseVideo
    | FastForward
    | FastRewind
    | GetVideoTime VideoTime
    | SetVideoTime VideoTime
    | SaveRecording
    | PlayRecording Recording
    | DeleteRecording Recording
    | LoadVideo VideoId
    | GotVideo (Result Http.Error Video)
    | JumpToSubtitle Subtitle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TabClicked tabId ->
            ( { model | tabId = tabId }, Cmd.none )

        ListenToVideo videoId ->
            ( { model
                | tabId = listenTabId
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
                        |> Maybe.andThen (getNextSubtitle model.videoTime)
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
                        |> Maybe.andThen (getPrevSubtitle model.videoTime)
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

        SaveRecording ->
            case getVideo model.videoId model.videos of
                Nothing ->
                    ( model, Cmd.none )

                Just video ->
                    case getSubtitleAt model.videoTime video.subtitles of
                        Nothing ->
                            ( model, Cmd.none )

                        Just subtitle ->
                            let
                                recordings =
                                    (model.recordings ++ [ subtitle ])
                                        |> List.unique
                                        |> List.sortWith
                                            (\a b ->
                                                if a.videoId /= b.videoId then
                                                    compare a.time b.time

                                                else
                                                    compare a.videoId b.videoId
                                            )
                            in
                            ( { model | recordings = recordings }, saveRecordings recordings )

        PlayRecording recording ->
            ( { model | videoIsPlaying = True }, playRecording recording )

        DeleteRecording deletedRecording ->
            let
                recordings =
                    List.remove deletedRecording model.recordings
            in
            ( { model | recordings = recordings }, saveRecordings recordings )

        LoadVideo videoId ->
            ( { model
                | videoId = Just videoId
                , videoIsPlaying = False
                , videoTime = 0
              }
            , loadVideo videoId
            )

        GotVideo response ->
            case response of
                Err _ ->
                    ( model, Cmd.none )

                Ok video ->
                    ( { model
                        | videos =
                            (model.videos ++ [ video ])
                                |> List.unique
                                |> List.sortBy
                                    (\v ->
                                        List.findIndex ((==) v.id) videoIds
                                            |> Maybe.withDefault -1
                                    )
                      }
                    , Cmd.none
                    )

        JumpToSubtitle subtitle ->
            ( model, jumpToSubtitle subtitle )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ class "fixed w-full" ] [ viewTabs model ]
        , Html.div [ class "pt-24 px-3" ]
            [ tabs
                |> List.getAt model.tabId
                |> Maybe.map (\tab -> tab.content model)
                |> Maybe.withDefault (Html.text "")
            ]
        ]


viewTabs : Model -> Html Msg
viewTabs model =
    Html.div [ class "h-16 flex text-xl bg-black" ]
        (List.indexedMap (viewTab model) tabs)


viewTab : Model -> TabId -> TabData -> Html Msg
viewTab model tabId tab =
    Html.button
        [ classList [ ( "text-cyan-400 border-b-2 border-cyan-400", model.tabId == tabId ) ]
        , class "grow h-full flex justify-center items-center cursor-pointer"
        , onClick (TabClicked tabId)
        ]
        [ Html.text tab.text ]


viewFindTab : Model -> Html Msg
viewFindTab model =
    Html.div []
        (List.map (viewVideoCard model) model.videos)


viewListenTab : Model -> Html Msg
viewListenTab model =
    case getVideo model.videoId model.videos of
        Nothing ->
            Html.div [ class "flex flex-col items-center" ]
                [ Html.div [ class "mb-2 text-xl" ] [ Html.text "No video selected" ]
                , Html.button
                    [ onClick (TabClicked findTabId)
                    , class "px-3 h-12 bg-cyan-500 hover:bg-cyan-600"
                    ]
                    [ Html.text "Find a video" ]
                ]

        Just video ->
            let
                currentSubtitle =
                    getSubtitleAt model.videoTime video.subtitles
                        |> Maybe.withDefault (Subtitle "" "" -1)
            in
            Html.div [ class "flex flex-col items-center gap-2 h-full" ]
                [ Html.div [ class "text-xl text-center" ]
                    [ Html.text video.title ]
                , Html.div [ class "w-full" ]
                    [ Html.input
                        [ Attr.type_ "range"
                        , Attr.min "0"
                        , Attr.max (String.fromFloat video.duration)
                        , Attr.step "1"
                        , Attr.value (String.fromFloat model.videoTime)
                        , onInput (String.toFloat >> Maybe.map SetVideoTime >> Maybe.withDefault (SetVideoTime 0))
                        , class "block w-full md:w-3/4 lg:w-1/2 mx-auto"
                        ]
                        []
                    ]
                , Html.div []
                    [ Html.text
                        (formatTime model.videoTime
                            ++ " / "
                            ++ formatTime video.duration
                        )
                    ]
                , Html.button [ onClick (JumpToSubtitle currentSubtitle) ]
                    [ Html.text currentSubtitle.text ]
                , Html.div [ class "flex gap-2" ]
                    [ Html.button
                        [ onClick FastRewind
                        , class "bg-cyan-500 w-12 h-12 hover:bg-cyan-600"
                        ]
                        [ labeledSymbol "Rewind" "<<" ]
                    , playButton model [ class "bg-cyan-500 w-12 h-12 hover:bg-cyan-600" ]
                    , Html.button
                        [ onClick FastForward
                        , class "bg-cyan-500 w-12 h-12 hover:bg-cyan-600"
                        ]
                        [ labeledSymbol "Fast-forward" ">>" ]
                    ]
                , Html.div []
                    [ Html.button
                        [ onClick SaveRecording
                        , class "bg-cyan-500 px-16 h-12 hover:bg-cyan-600"
                        ]
                        [ Html.text "Save" ]
                    ]
                , Html.div [ Attr.id subtitlesContainerId, class "overflow-y-scroll h-1/2 md:h-3/5" ]
                    (video.subtitles
                        |> List.map
                            (\subtitle ->
                                Html.div
                                    [ class "text-center"
                                    , classList
                                        [ ( "text-cyan-300", subtitle == currentSubtitle ) ]
                                    , onClick (SetVideoTime subtitle.time)
                                    , Attr.id (subtitleId subtitle)
                                    ]
                                    [ Html.text subtitle.text ]
                            )
                    )
                ]


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
    Html.span []
        [ Html.span [ attribute "aria-hidden" "true" ] [ Html.text symbol ]
        , Html.span [ class "sr-only" ] [ Html.text label ]
        ]


viewReviewTab : Model -> Html Msg
viewReviewTab model =
    if List.isEmpty model.recordings then
        Html.div [ class "text-center text-xl" ]
            [ Html.p [ class "mb-5" ] [ Html.text "No recordings saved yet." ]
            , Html.p []
                [ Html.text """Click the "Save" button in the "Listen" tab to save a recording.""" ]
            ]

    else
        Html.div [ class "grid gap-4 w-full md:w-3/4 lg:w-1/2 mx-auto" ]
            (List.map
                (\recording ->
                    Html.div [ class "grid gap-2 shadow shadow-white p-5" ]
                        [ Html.div []
                            [ getVideo (Just recording.videoId) model.videos
                                |> Maybe.map .title
                                |> Maybe.withDefault ""
                                |> Html.text
                            ]
                        , Html.div [] [ Html.text (formatTime recording.time) ]
                        , Html.div []
                            [ Html.text recording.text ]
                        , Html.div [ class "flex justify-between" ]
                            [ if Just recording.videoId == model.videoId then
                                Html.button
                                    [ onClick (PlayRecording recording)
                                    , class "px-5 h-12 bg-cyan-500 hover:bg-cyan-600"
                                    ]
                                    [ Html.text "Play" ]

                              else
                                Html.button
                                    [ onClick (LoadVideo recording.videoId)
                                    , class "px-5 h-12 bg-cyan-500 hover:bg-cyan-600"
                                    ]
                                    [ Html.text "Load video" ]
                            , Html.button
                                [ onClick (DeleteRecording recording)
                                , class "px-5 h-12 bg-cyan-500 hover:bg-cyan-600"
                                ]
                                [ Html.text "Delete" ]
                            ]
                        ]
                )
                model.recordings
            )


formatTime : Float -> String
formatTime totalSeconds =
    let
        seconds =
            floor totalSeconds |> modBy 60

        minutes =
            (floor totalSeconds // 60) |> modBy 60

        hours =
            floor totalSeconds // 60 // 60
    in
    [ hours, minutes, seconds ]
        |> List.map (String.fromInt >> String.padLeft 2 '0' >> String.right 2)
        |> String.join ":"


viewVideoCard : Model -> Video -> Html Msg
viewVideoCard model video =
    Html.div [ class "px-5 py-5 mb-4 shadow shadow-slate-100 mx-auto w-full md:w-3/4 lg:w-1/2" ]
        [ Html.div []
            [ Html.h2 [ class "text-xl mb-3" ] [ Html.text video.title ]
            , Html.div [ class "flex justify-between" ]
                [ Html.button
                    [ onClick (ListenToVideo video.id)
                    , class "px-3 h-12 bg-cyan-500 hover:bg-cyan-600"
                    ]
                    [ Html.text "Listen" ]
                , if model.videoId == Just video.id then
                    playButton model [ class "bg-cyan-500 hover:bg-cyan-600 w-12 h-12" ]

                  else
                    Html.text ""
                ]
            ]
        ]


playButton : Model -> List (Attribute Msg) -> Html Msg
playButton model attributes =
    if model.videoIsPlaying then
        Html.button (attributes ++ [ onClick PauseVideo ])
            [ labeledSymbol "Pause" "||" ]

    else
        Html.button (attributes ++ [ onClick PlayVideo ])
            [ labeledSymbol "Play" "▶" ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    getVideoTime GetVideoTime


port startVideo : String -> Cmd msg


port playVideo : () -> Cmd msg


port pauseVideo : () -> Cmd msg


port getVideoTime : (Float -> msg) -> Sub msg


port setVideoTime : Float -> Cmd msg


port loadVideo : String -> Cmd msg


port playRecording : Recording -> Cmd msg


port saveRecordings : List Recording -> Cmd msg


main : Program Json.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }