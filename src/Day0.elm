module Day0 exposing (part1, part2)


type alias Day0 =
    { part1 : String -> String
    }


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

part2 text =
    Debug.todo "Part 2 solution"