-- https://adventofcode.com/2019/day/5


module Day5 exposing (..)

import Array exposing (..)
import Html exposing (..)


type alias Program =
    Array Int


type alias Memory =
    { program : Program
    , input : List Int
    , output : List Int
    , address : Int -- pointer position
    }


type State
    = Processing Memory
    | Halted Memory


type Mode
    = Position
    | Immediate


main =
    text "Day 5"


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


type alias Instruction =
    { opcode : Int
    , modes : Int
    , params : List Int
    }


parseInstruction : Int -> Memory -> Instruction
parseInstruction address mem =
    let
        opcode =
            remainderBy 100 (read address mem.program)

        modes =
            read address mem.program // 100
    in
    { opcode = opcode
    , modes = modes
    , params = []
    }


doBinaryOp : Int -> Memory -> (Int -> Int -> Int) -> State
doBinaryOp opcodeAddress memory fn =
    -- Three position for binary op:
    -- the first two after opcodeAddress indicate the positions from which you should read the input values,
    -- and the third indicates the position at which the output should be stored.
    let
        parameter1Address =
            read (opcodeAddress + 1) memory.program

        parameter2Address =
            read (opcodeAddress + 2) memory.program

        resultAddress =
            read (opcodeAddress + 3) memory.program

        result =
            fn (read parameter1Address memory.program) (read parameter2Address memory.program)
    in
    Processing
        { memory
            | program = Array.set resultAddress result memory.program
            , address = memory.address + 4
        }


read : Int -> Program -> Int
read address program =
    program |> Array.get address |> Maybe.withDefault 0


readParamValue address mode program =
    case mode of
        0 ->
            read (read address program) program

        1 ->
            read address program

        _ ->
            0


runInstruction : Int -> Memory -> State
runInstruction opcodeAddress memory =
    let
        opcode =
            remainderBy 100 (read opcodeAddress memory.program)
    in
    case opcode of
        1 ->
            doBinaryOp opcodeAddress memory (+)

        2 ->
            doBinaryOp opcodeAddress memory (*)

        3 ->
            let
                resultAddress =
                    read (opcodeAddress + 1) memory.program

                inputValue =
                    List.head memory.input
            in
            case inputValue of
                Nothing ->
                    Halted memory

                Just val ->
                    Processing
                        { memory
                            | program = Array.set resultAddress val memory.program
                            , input = List.tail memory.input |> Maybe.withDefault []
                            , address = memory.address + 2
                        }

        4 ->
            let
                outputAddress =
                    read (opcodeAddress + 1) memory.program

                outputValue =
                    read outputAddress memory.program
            in
            Processing
                { memory
                    | output = List.append memory.output [ outputValue ]
                    , address = memory.address + 2
                }

        99 ->
            Halted memory

        _ ->
            Halted memory


runComputer : Memory -> Memory
runComputer memory =
    case runInstruction memory.address memory of
        Processing mem ->
            runComputer mem

        Halted mem ->
            mem


initMemory : List Int -> List Int -> Memory
initMemory intcode input =
    { program = Array.fromList intcode
    , input = input
    , output = []
    , address = 0
    }


test1 =
    initMemory [ 3, 0, 4, 0, 99 ] [ 333 ]
