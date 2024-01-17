module Flashcard exposing (Flashcard, decoder, equals, from, member, view)

import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)


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
    , showFlashcardBack : msg
    , passFlashcard : Flashcard -> msg
    , failFlashcard : Flashcard -> msg
    , goToPlayVideoTab : msg
    }


view : ViewProps msg -> Html msg
view props =
    case props.flashcards of
        [] ->
            div [ class "flex flex-col gap-4" ]
                [ p [ class "text-center text-xl" ]
                    [ text "You haven't saved any flashcards." ]
                , button
                    [ class "text-lg bg-blue-600 text-white py-2 px-4 w-full rounded-lg"
                    , onClick props.goToPlayVideoTab
                    ]
                    [ text "Save flashcards " ]
                ]

        flashcard :: _ ->
            div [ class "flex flex-col" ]
                [ div [ class "grow" ]
                    [ div [] [ text flashcard.traditional ]
                    , if flashcard.traditional /= flashcard.simplified then
                        div [] [ text flashcard.simplified ]

                      else
                        text ""
                    ]
                , if props.flashcardBackShown then
                    div [ class "grow" ]
                        [ div [] [ text flashcard.pinyin ]
                        , div [] [ text (String.join "; " flashcard.definitions) ]
                        , div []
                            [ button [ onClick (props.failFlashcard flashcard) ] [ text "Fail" ]
                            , button [ onClick (props.passFlashcard flashcard) ] [ text "Pass" ]
                            ]
                        ]

                  else
                    div [ class "grow" ]
                        [ button [ onClick props.showFlashcardBack ]
                            [ text "Show back" ]
                        ]
                ]
