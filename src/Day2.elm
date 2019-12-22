module Day2 exposing (..)

import Array exposing (..)
import Html exposing (..)


type alias Program =
    Array Int


type alias Memory =
    { program : Program
    , address : Int -- position of the pointer
    }


type State
    = Processing Memory
    | Halted Memory


main =
    text "Day 2"


toInt s =
    case String.toInt s of
        Just val ->
            val

        -- TODO: Handle this kind of error.
        Nothing ->
            0


parseCode str =
    String.split "," str
        |> List.map toInt


doBinaryOp : Int -> Memory -> (Int -> Int -> Int) -> State
doBinaryOp opcodeAddress memory fn =
    -- Three position for binary op:
    -- the first two after opcodeAddress indicate the positions from which you should read the input values,
    -- and the third indicates the position at which the output should be stored.
    let
        read address =
            memory.program |> Array.get address |> Maybe.withDefault 0

        parameter1Address =
            read (opcodeAddress + 1)

        parameter2Address =
            read (opcodeAddress + 2)

        resultAddress =
            read (opcodeAddress + 3)

        result =
            fn (read parameter1Address) (read parameter2Address)
    in
    Processing
        { memory
            | program = Array.set resultAddress result memory.program
            , address = memory.address + 4
        }


runInstruction : Int -> Memory -> State
runInstruction opcodeAddress memory =
    case Array.get opcodeAddress memory.program of
        Just 1 ->
            doBinaryOp opcodeAddress memory (+)

        Just 2 ->
            doBinaryOp opcodeAddress memory (*)

        Just 99 ->
            Halted memory

        _ ->
            Processing memory


runComputer : Memory -> Memory
runComputer memory =
    case runInstruction memory.address memory of
        Processing mem ->
            runComputer mem

        Halted mem ->
            mem


puzzleInput =
    [ 1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 1, 6, 19, 2, 19, 6, 23, 1, 23, 5, 27, 1, 9, 27, 31, 1, 31, 10, 35, 2, 35, 9, 39, 1, 5, 39, 43, 2, 43, 9, 47, 1, 5, 47, 51, 2, 51, 13, 55, 1, 55, 10, 59, 1, 59, 10, 63, 2, 9, 63, 67, 1, 67, 5, 71, 2, 13, 71, 75, 1, 75, 10, 79, 1, 79, 6, 83, 2, 13, 83, 87, 1, 87, 6, 91, 1, 6, 91, 95, 1, 10, 95, 99, 2, 99, 6, 103, 1, 103, 5, 107, 2, 6, 107, 111, 1, 10, 111, 115, 1, 115, 5, 119, 2, 6, 119, 123, 1, 123, 5, 127, 2, 127, 6, 131, 1, 131, 5, 135, 1, 2, 135, 139, 1, 139, 13, 0, 99, 2, 0, 14, 0 ]


prepareInput intcode noun verb =
    -- noun is program[1] and verb is program[2]
    let
        updateNounAndVerb ic =
            Array.set 2 verb (Array.set 1 noun ic)
    in
    { program = updateNounAndVerb (Array.fromList intcode)
    , address = 0
    }


makeRow : Int -> List ( Int, Int )
makeRow num =
    List.map (\ln -> ( num, ln )) (List.range 0 99)


checkResult intcode noun verb =
    let
        result =
            runComputer (prepareInput intcode noun verb)
    in
    Array.get 0 result.program


part2 : List ( Int, Int ) -> Int
part2 permutations =
    case permutations of
        [] ->
            -1

        ( n, v ) :: t ->
            if 19690720 == Maybe.withDefault 0 (checkResult puzzleInput n v) then
                (n * 100) + v

            else
                part2 t



-- > Part1: runComputer (prepareInput puzzleInput 12 2)
-- > Part2: part2 (List.concat (List.map makeRow (List.range 0 99)))
