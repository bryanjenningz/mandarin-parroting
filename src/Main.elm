port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, Html, button, div, h2, input, span, text)
import Html.Attributes as Attr exposing (attribute, class, classList)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import Task
import Video exposing (Subtitle, Video, VideoId)
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
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tab = SelectVideoTab
      , videoId = Nothing
      , videoIsPlaying = False
      , videoTime = 0
      , videos = []
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
                        |> Maybe.andThen (Video.nextSubtitle model.videoTime)
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
                        |> Maybe.andThen (Video.prevSubtitle model.videoTime)
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


getVideo : Maybe VideoId -> List Video -> Maybe Video
getVideo videoId videos =
    videoId |> Maybe.andThen (\id -> List.find (.id >> (==) id) videos)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "fixed w-full" ] [ viewTabs model ]
        , div [ class "pt-24 px-3" ]
            [ case model.tab of
                SelectVideoTab ->
                    viewSelectVideoTab model

                PlayVideoTab ->
                    viewPlayVideoTab model
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
        [ classList [ ( "text-cyan-400 border-b-2 border-cyan-400", model.tab == tab ) ]
        , class "grow h-full flex justify-center items-center cursor-pointer"
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
    div [] (List.map (viewVideoCard model) model.videos)


viewPlayVideoTab : Model -> Html Msg
viewPlayVideoTab model =
    case getVideo model.videoId model.videos of
        Nothing ->
            div [ class "flex flex-col items-center" ]
                [ div [ class "mb-2 text-xl" ] [ text "No video selected" ]
                , button
                    [ onClick (TabClicked SelectVideoTab)
                    , class "px-3 h-12 bg-cyan-500 hover:bg-cyan-600"
                    ]
                    [ text "Select a video" ]
                ]

        Just video ->
            let
                currentSubtitle =
                    Video.subtitleAt model.videoTime video.subtitles
                        |> Maybe.withDefault (Subtitle "" "" -1)
            in
            div [ class "flex flex-col items-center gap-2 h-full" ]
                [ div [ class "text-xl text-center" ]
                    [ text video.title ]
                , div [ class "w-full" ]
                    [ input
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
                , div []
                    [ text
                        (VideoTime.toString model.videoTime
                            ++ " / "
                            ++ VideoTime.toString video.duration
                        )
                    ]
                , button [ onClick (JumpToSubtitle currentSubtitle) ]
                    [ text currentSubtitle.text ]
                , div [ class "flex gap-2" ]
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
                , div [ Attr.id subtitlesContainerId, class "overflow-y-scroll h-1/2 md:h-3/5" ]
                    (video.subtitles
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
    span []
        [ span [ attribute "aria-hidden" "true" ] [ text symbol ]
        , span [ class "sr-only" ] [ text label ]
        ]


viewVideoCard : Model -> Video -> Html Msg
viewVideoCard model video =
    div [ class "px-5 py-5 mb-4 shadow shadow-slate-100 mx-auto w-full md:w-3/4 lg:w-1/2" ]
        [ div []
            [ h2 [ class "text-xl mb-3" ] [ text video.title ]
            , div [ class "flex justify-between" ]
                [ button
                    [ onClick (StartVideo video.id)
                    , class "px-3 h-12 bg-cyan-500 hover:bg-cyan-600"
                    ]
                    [ text "Listen" ]
                , if model.videoId == Just video.id then
                    playButton model [ class "bg-cyan-500 hover:bg-cyan-600 w-12 h-12" ]

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
    getVideoTime GetVideoTime


port startVideo : String -> Cmd msg


port playVideo : () -> Cmd msg


port pauseVideo : () -> Cmd msg


port getVideoTime : (VideoTime -> msg) -> Sub msg


port setVideoTime : VideoTime -> Cmd msg
