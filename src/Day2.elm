module Day2 exposing (..)

import Array exposing (..)
import Html exposing (..)


type alias Program =
    Array Int


type alias Memory =
    { prg : Program
    , pos : Int
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
doBinaryOp opcodePos m fn =
    -- Three position for binary op:
    -- the first two after opcodePos indicate the positions from which you should read the input values (args),
    -- and the third indicates the position at which the output should be stored (resultPosition).
    let
        getArgPos pointerPos =
            -- Returns zero if position is out of program range
            Maybe.withDefault 0 (Array.get pointerPos m.prg)

        getArg pointerPos =
            Maybe.withDefault 0 (Array.get (getArgPos pointerPos) m.prg)

        result =
            fn (getArg (opcodePos + 1)) (getArg (opcodePos + 2))

        resultPosition =
            getArgPos (opcodePos + 3)
    in
    Processing
        { m
            | prg = Array.set resultPosition result m.prg
            , pos = m.pos + 4
        }


processOpcode : Int -> Memory -> State
processOpcode opcodePos m =
    case Array.get opcodePos m.prg of
        Just 1 ->
            doBinaryOp opcodePos m (+)

        Just 2 ->
            doBinaryOp opcodePos m (*)

        Just 99 ->
            Halted m

        _ ->
            Processing m


runComputer : Memory -> Memory
runComputer mem =
    case processOpcode mem.pos mem of
        Processing m ->
            runComputer m

        Halted m ->
            m


day2input =
    -- (Replaced) Before running the program, replace position 1 with the value 12 and replace position 2 with the value 2.
    { prg = Array.fromList [ 1, 12, 2, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 1, 6, 19, 2, 19, 6, 23, 1, 23, 5, 27, 1, 9, 27, 31, 1, 31, 10, 35, 2, 35, 9, 39, 1, 5, 39, 43, 2, 43, 9, 47, 1, 5, 47, 51, 2, 51, 13, 55, 1, 55, 10, 59, 1, 59, 10, 63, 2, 9, 63, 67, 1, 67, 5, 71, 2, 13, 71, 75, 1, 75, 10, 79, 1, 79, 6, 83, 2, 13, 83, 87, 1, 87, 6, 91, 1, 6, 91, 95, 1, 10, 95, 99, 2, 99, 6, 103, 1, 103, 5, 107, 2, 6, 107, 111, 1, 10, 111, 115, 1, 115, 5, 119, 2, 6, 119, 123, 1, 123, 5, 127, 2, 127, 6, 131, 1, 131, 5, 135, 1, 2, 135, 139, 1, 139, 13, 0, 99, 2, 0, 14, 0 ]
    , pos = 0
    }
