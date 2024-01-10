port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = updateAndSave
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { counts : List Int }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        decodedFlags : Result Decode.Error Model
        decodedFlags =
            flags
                |> Decode.decodeValue Decode.string
                |> Result.andThen (Decode.decodeString modelDecoder)
    in
    case decodedFlags of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( { counts = [ 0 ] }, Cmd.none )


modelDecoder : Decoder Model
modelDecoder =
    Decode.map (\counts -> { counts = counts })
        (Decode.field "counts" (Decode.list Decode.int))



-- UPDATE


type Msg
    = AddCount
    | Increment Int
    | Decrement Int
    | Reset Int
    | Remove Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCount ->
            ( { model | counts = model.counts ++ [ 0 ] }, Cmd.none )

        Increment index ->
            ( { model | counts = List.updateAt index (\x -> x + 1) model.counts }, Cmd.none )

        Decrement index ->
            ( { model | counts = List.updateAt index (\x -> x - 1) model.counts }, Cmd.none )

        Reset index ->
            ( { model | counts = List.updateAt index (\_ -> 0) model.counts }, Cmd.none )

        Remove index ->
            ( { model | counts = List.removeAt index model.counts }, Cmd.none )


updateAndSave : Msg -> Model -> ( Model, Cmd Msg )
updateAndSave msg model =
    let
        ( newModel, cmd ) =
            update msg model
    in
    ( newModel, Cmd.batch [ cmd, save newModel ] )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-black text-white flex flex-col items-center" ]
        [ button [ class btnClass, onClick AddCount ] [ text "Add count" ]
        , div [] (List.indexedMap viewCount model.counts)
        ]


viewCount : Int -> Int -> Html Msg
viewCount index count =
    div [ class "flex flex-col items-center" ]
        [ div [ class "text-2xl" ] [ text (String.fromInt count) ]
        , div [ class "flex gap-1" ]
            [ button [ onClick (Decrement index), class btnClass ] [ text "-" ]
            , button [ onClick (Increment index), class btnClass ] [ text "+" ]
            , button [ onClick (Reset index), class btnClass ] [ text "o" ]
            , button [ onClick (Remove index), class btnClass ] [ text "x" ]
            ]
        ]


btnClass : String
btnClass =
    "py-2 px-4 bg-blue-600 rounded-lg hover:brightness-110"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


port save : Model -> Cmd msg
