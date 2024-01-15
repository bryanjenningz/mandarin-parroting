module Dictionary exposing (Model, Msg, init, update)

import Http



-- MODEL


type Model
    = EmptyDictionary
    | Dictionary { simplified : List String, traditional : List String }
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
                    simplified
                        |> List.sortBy
                            (\line ->
                                line
                                    |> parseLine
                                    |> Maybe.map .traditional
                                    |> Maybe.withDefault ""
                            )
            in
            ( Dictionary
                { simplified = simplified
                , traditional = traditional
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
