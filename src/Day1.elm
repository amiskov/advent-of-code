-- https://adventofcode.com/2019/day/1


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
