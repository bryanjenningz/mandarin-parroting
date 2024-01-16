module Flashcard exposing (Flashcard, decoder, equals, from, member)

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
