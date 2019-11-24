module Day0 exposing (part1)


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
