module Dictionary exposing (Model, Msg, init, search, update)

import Array exposing (Array)
import Http



-- MODEL


type Model
    = EmptyDictionary
    | Dictionary { simplified : Array String, traditional : Array String }
    | FailedToLoadDictionary Http.Error


init : ( Model, Cmd Msg )
init =
    ( EmptyDictionary, fetch )


fetch : Cmd Msg
fetch =
    Http.get
        { url = "/dictionary.txt"
        , expect = Http.expectString LoadDictionary
        }



-- UPDATE


type Msg
    = LoadDictionary (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        LoadDictionary (Err error) ->
            ( FailedToLoadDictionary error, Cmd.none )

        LoadDictionary (Ok dictionary) ->
            let
                simplified =
                    dictionary |> String.split "\n"

                traditional =
                    simplified |> List.sortBy toTraditional
            in
            ( Dictionary
                { simplified = Array.fromList simplified
                , traditional = Array.fromList traditional
                }
            , Cmd.none
            )



-- LINE


type alias Line =
    { simplified : String
    , traditional : String
    , pinyin : String
    , definitions : List String
    }


parseLine : String -> Maybe Line
parseLine line =
    case line |> String.split "\t" of
        [ simplified, traditional, pinyin, definitions ] ->
            Just
                { simplified = simplified
                , traditional = traditional
                , pinyin = pinyin
                , definitions = definitions |> String.split "; "
                }

        _ ->
            Nothing


search : String -> Model -> Maybe Line
search searchText model =
    case model of
        EmptyDictionary ->
            Nothing

        FailedToLoadDictionary _ ->
            Nothing

        Dictionary dictionaryData ->
            case search_ searchText toSimplified dictionaryData.simplified of
                Nothing ->
                    search_ searchText toTraditional dictionaryData.traditional

                Just line ->
                    Just line


search_ : String -> (String -> String) -> Array String -> Maybe Line
search_ searchText toSortKey dictionaryData =
    Nothing


toSimplified : String -> String
toSimplified line =
    line
        |> parseLine
        |> Maybe.map .simplified
        |> Maybe.withDefault ""


toTraditional : String -> String
toTraditional line =
    line
        |> parseLine
        |> Maybe.map .traditional
        |> Maybe.withDefault ""
