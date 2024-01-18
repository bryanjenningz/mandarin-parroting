module Flashcard exposing (Flashcard, decoder, encoder, equals, from, member, view)

import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ProgressBar exposing (ProgressBar)


type alias Flashcard =
    { traditional : String
    , simplified : String
    , pinyin : String
    , definitions : List String
    , correctReviewsInARow : Maybe Int
    }


decoder : Decoder Flashcard
decoder =
    Decode.map5
        (\traditional simplified pinyin definitions correctReviewsInARow ->
            { traditional = traditional
            , simplified = simplified
            , pinyin = pinyin
            , definitions = definitions
            , correctReviewsInARow = correctReviewsInARow
            }
        )
        (Decode.field "traditional" Decode.string)
        (Decode.field "simplified" Decode.string)
        (Decode.field "pinyin" Decode.string)
        (Decode.field "definitions" (Decode.list Decode.string))
        (Decode.field "correctReviewsInARow" (Decode.maybe Decode.int))


encoder : Flashcard -> Encode.Value
encoder flashcard =
    Encode.object
        [ ( "traditional", Encode.string flashcard.traditional )
        , ( "simplified", Encode.string flashcard.simplified )
        , ( "pinyin", Encode.string flashcard.pinyin )
        , ( "definitions", Encode.list Encode.string flashcard.definitions )
        , ( "correctReviewsInARow"
          , flashcard.correctReviewsInARow
                |> Maybe.map Encode.int
                |> Maybe.withDefault Encode.null
          )
        ]


from :
    { traditional : String
    , simplified : String
    , pinyin : String
    , definitions : List String
    }
    -> Flashcard
from props =
    { traditional = props.traditional
    , simplified = props.simplified
    , pinyin = props.pinyin
    , definitions = props.definitions
    , correctReviewsInARow = Nothing
    }


member : { a | traditional : String } -> List Flashcard -> Bool
member flashcard flashcards =
    List.member flashcard.traditional (List.map .traditional flashcards)


equals : Flashcard -> Flashcard -> Bool
equals a b =
    a.traditional == b.traditional


type alias ViewProps msg =
    { flashcardBackShown : Bool
    , flashcards : List Flashcard
    , showFlashcardBack : Flashcard -> msg
    , passFlashcard : Flashcard -> msg
    , failFlashcard : Flashcard -> msg
    , deleteFlashcard : Flashcard -> msg
    , goToPlayVideoTab : msg
    , playTextToSpeech : String -> msg
    , progressBar : ProgressBar
    }


view : ViewProps msg -> Html msg
view props =
    case sort props.flashcards of
        [] ->
            div [ class "flex flex-col gap-4" ]
                [ p [ class "text-center text-xl" ]
                    [ text "You haven't saved any flashcards." ]
                , button
                    [ class "text-lg bg-blue-600 text-white py-2 px-4 w-full rounded-lg"
                    , onClick props.goToPlayVideoTab
                    ]
                    [ text "Save flashcards" ]
                ]

        flashcard :: _ ->
            div [ class "flex flex-col grow" ]
                [ div [ class "flex flex-col gap-2 mb-2" ]
                    [ ProgressBar.view ProgressBar.FlashcardsSavedMode props.progressBar
                    , ProgressBar.view ProgressBar.FlashcardsReviewedMode props.progressBar
                    ]
                , div [ class "relative flex justify-center items-center gap-3 text-3xl" ]
                    [ div [] [ text flashcard.traditional ]
                    , if flashcard.traditional /= flashcard.simplified then
                        div [] [ text flashcard.simplified ]

                      else
                        text ""
                    , button
                        [ onClick (props.deleteFlashcard flashcard)
                        , class "absolute top-0 right-0 w-8 h-8 flex justify-center items-center text-4xl"
                        ]
                        [ text "×" ]
                    ]
                , if props.flashcardBackShown then
                    div [ class "flex flex-col justify-between items-center grow" ]
                        [ div [ class "flex flex-col gap-4" ]
                            [ button [ onClick (props.playTextToSpeech flashcard.traditional) ]
                                [ text "🔊" ]
                            , div [ class "text-2xl text-center" ] [ text flashcard.pinyin ]
                            , div [ class "text-xl text-center" ] [ text (String.join "; " flashcard.definitions) ]
                            ]
                        , div [ class "w-full flex gap-3 text-lg" ]
                            [ button
                                [ onClick (props.failFlashcard flashcard)
                                , class "grow basis-1 py-2 px-4 bg-red-600 rounded-lg uppercase"
                                ]
                                [ text "Fail" ]
                            , button
                                [ onClick (props.passFlashcard flashcard)
                                , class "grow basis-1 py-2 px-4 bg-blue-600 rounded-lg uppercase"
                                ]
                                [ text "Pass" ]
                            ]
                        ]

                  else
                    div [ class "flex flex-col justify-end grow" ]
                        [ button
                            [ onClick (props.showFlashcardBack flashcard)
                            , class "py-2 px-4 bg-blue-600 text-lg rounded-lg uppercase"
                            ]
                            [ text "Show back" ]
                        ]
                ]



-- INTERNAL


sort : List Flashcard -> List Flashcard
sort flashcards =
    List.sortBy
        (\card -> card.correctReviewsInARow |> Maybe.withDefault 1)
        flashcards
