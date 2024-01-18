module ProgressBar exposing (ProgressBar, decoder, encoder, incrementSavedFlashcardsToday, init, setNow, subscriptions, view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as List
import Time


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


setNow : Time.Posix -> ProgressBar -> ProgressBar
setNow now (ProgressBar data) =
    if toDate now == toDate data.now then
        ProgressBar { data | now = now }

    else
        ProgressBar { data | now = now, savedFlashcardsToday = 0 }


incrementSavedFlashcardsToday : ProgressBar -> ProgressBar
incrementSavedFlashcardsToday (ProgressBar data) =
    ProgressBar { data | savedFlashcardsToday = data.savedFlashcardsToday + 1 }


view : ProgressBar -> Html msg
view (ProgressBar data) =
    div [ class "relative w-full bg-slate-500 rounded-full" ]
        [ div
            [ class "absolute left-0 top-0 bottom-0 bg-blue-600"
            , style "width"
                (percent data.savedFlashcardsToday
                    (flashcardGoal data.savedFlashcardsToday)
                )
            ]
            []
        ]


subscriptions : (ProgressBar -> msg) -> ProgressBar -> Sub msg
subscriptions toMsg (ProgressBar data) =
    Time.every (60 * 1000) (\now -> ProgressBar { data | now = now } |> toMsg)



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
