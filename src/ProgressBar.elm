module ProgressBar exposing (ProgressBar, decoder, encoder, incrementSavedFlashcardsToday, init, subscriptions, view)

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
    , savedFlashcardsToday : Int
    }


init : ProgressBar
init =
    ProgressBar
        { now = Time.millisToPosix 0
        , savedFlashcardsToday = 0
        }



-- ENCODING / DECODING


decoder : Decoder ProgressBar
decoder =
    Decode.map2
        (\now savedFlashcardsToday ->
            ProgressBar
                { now = Time.millisToPosix now
                , savedFlashcardsToday = savedFlashcardsToday
                }
        )
        (Decode.field "now" Decode.int)
        (Decode.field "savedFlashcardsToday" Decode.int)


encoder : ProgressBar -> Encode.Value
encoder (ProgressBar data) =
    Encode.object
        [ ( "now", Encode.int (Time.posixToMillis data.now) )
        , ( "savedFlashcardsToday", Encode.int data.savedFlashcardsToday )
        ]



-- HELPERS


incrementSavedFlashcardsToday : ProgressBar -> ProgressBar
incrementSavedFlashcardsToday (ProgressBar data) =
    ProgressBar { data | savedFlashcardsToday = data.savedFlashcardsToday + 1 }



-- VIEW


view : ProgressBar -> Html msg
view (ProgressBar data) =
    div [ class "relative w-full bg-slate-500 rounded-full h-4 overflow-hidden" ]
        [ div
            [ class "absolute left-0 top-0 bottom-0 bg-blue-600"
            , style "width"
                (percent data.savedFlashcardsToday
                    (flashcardGoal data.savedFlashcardsToday)
                )
            ]
            []
        , div [ class "absolute inset-0 flex justify-center items-center text-xs" ]
            [ text <|
                String.fromInt data.savedFlashcardsToday
                    ++ " / "
                    ++ String.fromInt (flashcardGoal data.savedFlashcardsToday)
                    ++ " flashcards created"
            ]
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
        ProgressBar { data | now = now, savedFlashcardsToday = 0 }
