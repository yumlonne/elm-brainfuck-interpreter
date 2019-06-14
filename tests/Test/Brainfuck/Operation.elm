module Test.Brainfuck.Operation exposing (..)

import Brainfuck.Operation exposing(Operation(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
    describe "The Brainfuck.Operation module"
        [ describe "Brainfuck.Operation.parse"
            [ test "should parse correct Brainfuck Program" <|
                \run ->
                    let
                        program = "+-++<.,.>"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [VInc, VDec, VInc, VInc, PDec, Print, Read, Print, PInc])

            , test "should parse contain While-loop Brainfuck Program" <|
                \run ->
                    let
                        program = "[+]--<"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [While [VInc], VDec, VDec, PDec])

            , test "should parse contain nested While-loop Brainfuck Program" <|
                \run ->
                    let
                        program = "+[[>]<+>[[+]]]"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [VInc, While [While [PInc], PDec, VInc, PInc, While [While [VInc]]]])

            , test "should ignore non-program characters" <|
                \run ->
                    let
                        program = "hoge+fuga<?-"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [VInc, PDec, VDec])
            ]
        ]
