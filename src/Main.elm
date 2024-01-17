port module Main exposing (main)

import Browser
import Dictionary
import Flags exposing (Flags)
import Flashcard exposing (Flashcard)
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode exposing (Value)
import NewVideo exposing (NewVideo)
import Subtitle exposing (Subtitle)
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
    , dictionary : Dictionary.Model
    , dictionaryLookup : Maybe ( Subtitle, Int )
    , flashcards : List Flashcard
    , flashcardBackShown : Bool
    }


init : Value -> ( Model, Cmd Msg )
init value =
    let
        flags =
            Flags.decode value
    in
    ( { tab = SelectVideoTab
      , videoId = flags.videoId
      , videoIsPlaying = False
      , videoTime = 0
      , videoSpeed = flags.videoSpeed
      , videos = flags.videos
      , newVideo = NewVideo.empty
      , newVideoError = Nothing
      , dictionary = Dictionary.init
      , dictionaryLookup = Nothing
      , flashcards = flags.flashcards
      , flashcardBackShown = False
      }
    , Dictionary.fetch DictionaryMsg
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
    | DictionaryMsg Dictionary.Msg
    | SetDictionaryLookup (Maybe ( Subtitle, Int ))
    | SaveFlashcard Flashcard
    | DeleteFlashcard Flashcard
    | ShowFlashcardBack
    | PassFlashcard Flashcard
    | FailFlashcard Flashcard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TabClicked tab ->
            ( { model | tab = tab }
            , Video.getById model.videoId model.videos
                |> Maybe.andThen (\video -> Subtitle.at model.videoTime video.subtitles)
                |> Maybe.map (\subtitle -> Subtitle.jumpTo { subtitle = subtitle, noop = NoOp })
                |> Maybe.withDefault Cmd.none
            )

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
                    , flashcards = newModel.flashcards
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
                        |> Maybe.andThen (Subtitle.next model.videoTime)
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
                        |> Maybe.andThen (Subtitle.prev model.videoTime)
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
            ( model, Subtitle.jumpTo { subtitle = subtitle, noop = NoOp } )

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
                    , flashcards = newModel.flashcards
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
                , flashcards = newModel.flashcards
                }
            )

        DictionaryMsg dictionaryMsg ->
            ( { model | dictionary = Dictionary.update dictionaryMsg model.dictionary }
            , Cmd.none
            )

        SetDictionaryLookup dictionaryLookup ->
            ( { model | dictionaryLookup = dictionaryLookup }, Cmd.none )

        SaveFlashcard flashcard ->
            let
                newModel =
                    { model
                        | flashcards =
                            List.filter (\card -> not <| Flashcard.equals card flashcard) model.flashcards
                                ++ [ flashcard ]
                    }
            in
            ( newModel
            , saveFlags
                { videoId = newModel.videoId
                , videoSpeed = newModel.videoSpeed
                , videos = newModel.videos
                , flashcards = newModel.flashcards
                }
            )

        DeleteFlashcard flashcard ->
            let
                newModel =
                    { model
                        | flashcards =
                            List.filter (\card -> not <| Flashcard.equals card flashcard) model.flashcards
                    }
            in
            ( newModel
            , saveFlags
                { videoId = newModel.videoId
                , videoSpeed = newModel.videoSpeed
                , videos = newModel.videos
                , flashcards = newModel.flashcards
                }
            )

        ShowFlashcardBack ->
            ( { model | flashcardBackShown = True }, Cmd.none )

        PassFlashcard flashcard ->
            let
                newModel =
                    { model
                        | flashcards =
                            List.map
                                (\card ->
                                    if Flashcard.equals card flashcard then
                                        { card
                                            | correctReviewsInARow =
                                                card.correctReviewsInARow
                                                    |> Maybe.withDefault 0
                                                    |> (+) 1
                                                    |> Just
                                        }

                                    else
                                        card
                                )
                                model.flashcards
                        , flashcardBackShown = False
                    }
            in
            ( newModel
            , saveFlags
                { videoId = newModel.videoId
                , videoSpeed = newModel.videoSpeed
                , videos = newModel.videos
                , flashcards = newModel.flashcards
                }
            )

        FailFlashcard flashcard ->
            let
                newModel =
                    { model
                        | flashcards =
                            List.map
                                (\card ->
                                    if Flashcard.equals card flashcard then
                                        { card | correctReviewsInARow = Just 0 }

                                    else
                                        card
                                )
                                model.flashcards
                        , flashcardBackShown = False
                    }
            in
            ( newModel
            , saveFlags
                { videoId = newModel.videoId
                , videoSpeed = newModel.videoSpeed
                , videos = newModel.videos
                , flashcards = newModel.flashcards
                }
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center" ]
        [ div [ class "fixed w-full" ] [ viewTabs model ]
        , div [ class "w-full max-w-2xl" ]
            [ div [ class "pt-20 px-3" ]
                [ case model.tab of
                    SelectVideoTab ->
                        viewSelectVideoTab model

                    PlayVideoTab ->
                        viewPlayVideoTab model

                    ReviewTab ->
                        viewReviewTab model
                ]
            ]
        ]


type Tab
    = SelectVideoTab
    | PlayVideoTab
    | ReviewTab


tabs : List Tab
tabs =
    [ SelectVideoTab
    , PlayVideoTab
    , ReviewTab
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

                ReviewTab ->
                    "Review"
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
                    [ h2 [ class "text-2xl text-center" ]
                        [ text <|
                            if List.isEmpty model.videos then
                                "You haven't added any videos yet."

                            else
                                "You haven't selected a video to practice."
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
                    Subtitle.at model.videoTime video.subtitles
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
                        Subtitle.view
                            { currentSubtitle = subtitle
                            , subtitles = video.subtitles
                            , setVideoTime = SetVideoTime
                            , dictionary = model.dictionary
                            , dictionaryLookup = model.dictionaryLookup
                            , setDictionaryLookup = SetDictionaryLookup
                            , flashcards = model.flashcards
                            , saveFlashcard = SaveFlashcard
                            , deleteFlashcard = DeleteFlashcard
                            }
                ]


viewReviewTab : Model -> Html Msg
viewReviewTab model =
    div []
        (case model.flashcards of
            [] ->
                [ div [ class "flex flex-col gap-4" ]
                    [ div [ class "text-center text-xl" ]
                        [ text "You haven't saved any flashcards." ]
                    , button
                        [ class "text-lg bg-blue-600 text-white py-2 px-4 w-full rounded-lg"
                        , onClick (TabClicked PlayVideoTab)
                        ]
                        [ text "Save flashcards " ]
                    ]
                ]

            _ ->
                List.map
                    (\flashcard ->
                        div [ class "p-4 flex flex-col gap-3" ]
                            [ div [ class "flex gap-3" ]
                                [ div [] [ text flashcard.traditional ]
                                , if flashcard.simplified /= flashcard.traditional then
                                    div [] [ text flashcard.simplified ]

                                  else
                                    text ""
                                ]
                            , div [] [ text flashcard.pinyin ]
                            , div [] [ text (String.join "; " flashcard.definitions) ]
                            ]
                    )
                    model.flashcards
        )



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
