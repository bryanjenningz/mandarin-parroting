module ProgressBar exposing (ProgressBar, ProgressBarMode(..), decoder, encoder, incrementFlashcardsSaved, init, subscriptions, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as List
import Time



-- MODEL


type ProgressBar
    = ProgressBar ProgressBarData


type alias ProgressBarData =
    { now : Time.Posix
    , flashcardsSaved : Int
    , flashcardsReviewed : Int
    }


init : ProgressBar
init =
    ProgressBar
        { now = Time.millisToPosix 0
        , flashcardsSaved = 0
        , flashcardsReviewed = 0
        }



-- ENCODING / DECODING


decoder : Decoder ProgressBar
decoder =
    Decode.map3
        (\now flashcardsSaved flashcardsReviewed ->
            ProgressBar
                { now = Time.millisToPosix now
                , flashcardsSaved = flashcardsSaved
                , flashcardsReviewed = flashcardsReviewed
                }
        )
        (Decode.field "now" Decode.int)
        (Decode.field "flashcardsSaved" Decode.int)
        (Decode.field "flashcardsReviewed" Decode.int)


encoder : ProgressBar -> Encode.Value
encoder (ProgressBar data) =
    Encode.object
        [ ( "now", Encode.int (Time.posixToMillis data.now) )
        , ( "flashcardsSaved", Encode.int data.flashcardsSaved )
        , ( "flashcardsReviewed", Encode.int data.flashcardsReviewed )
        ]



-- HELPERS


incrementFlashcardsSaved : ProgressBar -> ProgressBar
incrementFlashcardsSaved (ProgressBar data) =
    ProgressBar { data | flashcardsSaved = data.flashcardsSaved + 1 }



-- VIEW


type ProgressBarMode
    = FlashcardsCreatedMode
    | FlashcardsReviewedMode


view : ProgressBarMode -> ProgressBar -> Html msg
view mode (ProgressBar data) =
    let
        { width, textLabel } =
            case mode of
                FlashcardsCreatedMode ->
                    { width =
                        percent data.flashcardsSaved
                            (flashcardGoal data.flashcardsSaved)
                    , textLabel =
                        String.fromInt data.flashcardsSaved
                            ++ " / "
                            ++ String.fromInt (flashcardGoal data.flashcardsSaved)
                            ++ " flashcards created"
                    }

                FlashcardsReviewedMode ->
                    { width =
                        percent data.flashcardsReviewed
                            (flashcardGoal data.flashcardsReviewed)
                    , textLabel =
                        String.fromInt data.flashcardsSaved
                            ++ " / "
                            ++ String.fromInt (flashcardGoal data.flashcardsSaved)
                            ++ " flashcards created"
                    }
    in
    div [ class "relative w-full bg-slate-500 rounded-full h-4 overflow-hidden" ]
        [ div
            [ class "absolute left-0 top-0 bottom-0 bg-blue-600"
            , style "width" width
            ]
            []
        , div [ class "absolute inset-0 flex justify-center items-center text-xs" ]
            [ text textLabel ]
        ]



-- SUBSCRIPTIONS


subscriptions : (ProgressBar -> msg) -> ProgressBar -> Sub msg
subscriptions toMsg progressBar =
    Time.every (60 * 1000) (\now -> setNow now progressBar |> toMsg)



-- INTERNAL


type alias Date =
    { year : Int
    , month : Time.Month
    , day : Int
    }


toDate : Time.Posix -> Date
toDate posix =
    { year = Time.toYear Time.utc posix
    , month = Time.toMonth Time.utc posix
    , day = Time.toDay Time.utc posix
    }


flashcardGoals : List Int
flashcardGoals =
    [ 1, 5, 10, 15, 20, 25, 50, 100, 200, 500, 1000 ]


flashcardGoal : Int -> Int
flashcardGoal flashcardsSavedToday =
    flashcardGoals
        |> List.find (\goal -> goal > flashcardsSavedToday)
        |> Maybe.withDefault flashcardsSavedToday


percent : Int -> Int -> String
percent numerator denominator =
    String.fromInt (numerator * 100 // denominator) ++ "%"


setNow : Time.Posix -> ProgressBar -> ProgressBar
setNow now (ProgressBar data) =
    if toDate now == toDate data.now then
        ProgressBar { data | now = now }

    else
        ProgressBar
            { now = now
            , flashcardsSaved = 0
            , flashcardsReviewed = 0
            }
