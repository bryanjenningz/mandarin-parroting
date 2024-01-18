module ProgressBar exposing (ProgressBar, incrementSavedFlashcardsToday, resetFlashcardsSavedToday, setHereNow, view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import List.Extra as List
import Time


type ProgressBar
    = ProgressBar ProgressBarData


type alias ProgressBarData =
    { here : Time.Zone
    , now : Time.Posix
    , savedFlashcardsToday : Int
    }


setHereNow : { here : Time.Zone, now : Time.Posix } -> ProgressBar -> ProgressBar
setHereNow { here, now } (ProgressBar data) =
    ProgressBar { data | here = here, now = now }


incrementSavedFlashcardsToday : ProgressBar -> ProgressBar
incrementSavedFlashcardsToday (ProgressBar data) =
    ProgressBar { data | savedFlashcardsToday = data.savedFlashcardsToday + 1 }


resetFlashcardsSavedToday : ProgressBar -> ProgressBar
resetFlashcardsSavedToday (ProgressBar data) =
    ProgressBar { data | savedFlashcardsToday = 0 }


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



-- INTERNAL


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
