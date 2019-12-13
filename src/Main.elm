module Main exposing (main)

import Array exposing (Array, map)
import Browser
import Day0
import Day1
import Day2
import Html exposing (Html, button, div, pre, text)
import Html.Events exposing (onClick)
import Http


days =
    Array.fromList [ ( Day0.part1, Day0.part2 ) ]


type Model
    = Silence
    | Failure
    | Loading
    | Success String


type Msg
    = GotText (Result Http.Error String)
    | LoadDay Int


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Silence
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadDay n ->
            loadDay n

        GotText result ->
            case result of
                Ok inputData ->
                    let
                        answer =
                            Array.get n inputData
                    in
                    ( Success answer, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


loadDay dayNum =
    let
        url =
            "inputs/day" ++ String.fromInt dayNum ++ ".txt"
    in
    ( Loading
    , Http.get
        { url = url
        , expect = Http.expectString GotText
        }
    )


view : Model -> Html Msg
view model =
    div []
        (List.append (viewDayButtons 0) [ viewAnswer model ])


viewDayButtons n =
    let
        createButton i =
            button [ onClick (LoadDay i) ] [ text ("Solve Day " ++ String.fromInt i) ]
    in
    List.range 0 n
        |> List.map createButton


viewAnswer model =
    case model of
        Silence ->
            text ""

        Failure ->
            text "Sorry, can't load input data."

        Loading ->
            text "Loading..."

        Success answer ->
            text answer


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
