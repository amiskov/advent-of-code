module Day0 exposing (part1)


part1 text =
    let
        toInt s =
            case String.toInt s of
                Just n ->
                    n

                Nothing ->
                    0
    in
    text
        |> String.split "\n"
        |> List.map toInt
        |> List.foldl (\n acc -> acc + n) 0
        |> String.fromInt
