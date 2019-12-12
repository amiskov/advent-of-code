module Day1 exposing (..)

import Html exposing (text)


part1 text =
    let
        toInt s =
            Maybe.withDefault 0 (String.toInt s)
    in
    text
        |> String.split "\n"
        |> List.map toInt
        |> List.sum
        |> String.fromInt
