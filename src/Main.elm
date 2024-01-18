port module Main exposing (main)

import Browser
import Dictionary
import Flags
import Flashcard exposing (Flashcard)
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import NewVideo exposing (NewVideo)
import ProgressBar exposing (ProgressBar)
import Subtitle exposing (Subtitle)
import Video exposing (Video, VideoId)
import VideoTime exposing (VideoTime)


main : Program Decode.Value Model Msg
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
    , progressBar : ProgressBar
    }


init : Decode.Value -> ( Model, Cmd Msg )
init value =
    let
        flags =
            Flags.decode value
    in
    ( { tab = VideosTab
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
      , progressBar = flags.progressBar
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
    | DeleteVideo Video
    | DictionaryMsg Dictionary.Msg
    | SetDictionaryLookup (Maybe ( Subtitle, Int ))
    | SaveFlashcard Flashcard
    | DeleteFlashcard Flashcard
    | ShowFlashcardBack Flashcard
    | PassFlashcard Flashcard
    | FailFlashcard Flashcard
    | PlayTextToSpeech String
    | SetProgressBar ProgressBar


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TabClicked tab ->
            ( { model | tab = tab, flashcardBackShown = False }
            , Video.getById model.videoId model.videos
                |> Maybe.andThen (\video -> Subtitle.at model.videoTime video.subtitles)
                |> Maybe.map (\subtitle -> Subtitle.jumpTo { subtitle = subtitle, noop = NoOp })
                |> Maybe.withDefault Cmd.none
            )

        StartVideo videoId ->
            let
                newModel =
                    { model
                        | tab = PracticeTab
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
                , saveModel newModel
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
                , saveModel newModel
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
            ( newModel, saveModel newModel )

        DeleteVideo video ->
            let
                newModel =
                    { model | videos = List.filter (\x -> x.videoId /= video.videoId) model.videos }
            in
            ( newModel, saveModel newModel )

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
                        , progressBar = ProgressBar.incrementFlashcardsSaved model.progressBar
                    }
            in
            ( newModel, saveModel newModel )

        DeleteFlashcard flashcard ->
            let
                newModel =
                    { model
                        | flashcards =
                            List.filter (\card -> not <| Flashcard.equals card flashcard) model.flashcards
                        , flashcardBackShown = False
                    }
            in
            ( newModel, saveModel newModel )

        ShowFlashcardBack flashcard ->
            ( { model | flashcardBackShown = True }, textToSpeech flashcard.traditional )

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
                        , progressBar = ProgressBar.incrementFlashcardsReviewed model.progressBar
                    }
            in
            ( newModel, saveModel newModel )

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
                        , progressBar = ProgressBar.incrementFlashcardsReviewed model.progressBar
                    }
            in
            ( newModel, saveModel newModel )

        PlayTextToSpeech textString ->
            ( model, textToSpeech textString )

        SetProgressBar progressBar ->
            ( { model | progressBar = progressBar }, Cmd.none )


saveModel : Model -> Cmd Msg
saveModel model =
    { videoId = model.videoId
    , videoSpeed = model.videoSpeed
    , videos = model.videos
    , flashcards = model.flashcards
    , progressBar = model.progressBar
    }
        |> Flags.encode
        |> saveFlags



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center min-h-screen" ]
        [ div [ class "fixed w-full" ] [ viewTabs model ]
        , div [ class "w-full max-w-2xl flex flex-col grow pt-20 px-4 pb-4" ]
            [ case model.tab of
                VideosTab ->
                    viewSelectVideoTab model

                PracticeTab ->
                    viewPlayVideoTab model

                ReviewTab ->
                    viewReviewTab model
            ]
        ]


type Tab
    = VideosTab
    | PracticeTab
    | ReviewTab


tabs : List Tab
tabs =
    [ VideosTab
    , PracticeTab
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
                VideosTab ->
                    "Videos"

                PracticeTab ->
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
                        [ onClick (TabClicked VideosTab)
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
                [ div [ class "relative w-full" ]
                    [ ProgressBar.view ProgressBar.TimeListenedMode model.progressBar ]
                , div [ class "relative w-full" ]
                    [ ProgressBar.view ProgressBar.FlashcardsSavedMode model.progressBar ]
                , div [ class "text-xl text-center" ] [ text video.title ]
                , Video.viewSlider
                    { videoTime = model.videoTime
                    , setVideoTime = SetVideoTime
                    }
                    video
                , div [ class "text-xs" ]
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
    Flashcard.view
        { flashcardBackShown = model.flashcardBackShown
        , flashcards = model.flashcards
        , showFlashcardBack = ShowFlashcardBack
        , passFlashcard = PassFlashcard
        , failFlashcard = FailFlashcard
        , deleteFlashcard = DeleteFlashcard
        , goToPlayVideoTab = TabClicked PracticeTab
        , playTextToSpeech = PlayTextToSpeech
        , progressBar = model.progressBar
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ getVideoTime GetVideoTime
        , addVideo AddVideo
        , ProgressBar.subscriptions
            { setProgressBar = SetProgressBar
            , progressBar = model.progressBar
            , videoIsPlaying = model.videoIsPlaying
            }
        ]



-- PORTS


port startVideo : VideoId -> Cmd msg


port playVideo : () -> Cmd msg


port pauseVideo : () -> Cmd msg


port getVideoTime : (VideoTime -> msg) -> Sub msg


port setVideoTime : VideoTime -> Cmd msg


port setVideoSpeed : Int -> Cmd msg


port submitNewVideo : { videoId : String, subtitles : List Subtitle } -> Cmd msg


port addVideo : (Video -> msg) -> Sub msg


port textToSpeech : String -> Cmd msg


port saveFlags : Encode.Value -> Cmd msg
