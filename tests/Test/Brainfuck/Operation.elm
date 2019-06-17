module Test.Brainfuck.Operation exposing (suite)

import Brainfuck.Operation exposing (Operation(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Brainfuck.Operation module"
        [ describe "parse"
            [ test "brainfuckプログラムをパースできる" <|
                \run ->
                    let
                        program =
                            "+-++<.,.>"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [ VAdd 2, PAdd -1, Print, Read, Print, PAdd 1 ])
            , test "whileループが含まれていてもOK" <|
                \run ->
                    let
                        program =
                            "[+]--<"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [ While [ VAdd 1 ], VAdd -2, PAdd -1 ])
            , test "ネストループが含まれていてもOK" <|
                \run ->
                    let
                        program =
                            "+[[>]<+>[[+]]]"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [ VAdd 1, While [ While [ PAdd 1 ], PAdd -1, VAdd 1, PAdd 1, While [ While [ VAdd 1 ] ] ] ])
            , test "特定の記号以外は無視する" <|
                \run ->
                    let
                        program =
                            "hoge+fuga<?-"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [ VAdd 1, PAdd -1, VAdd -1 ])
            , test "連続する同方向への値操作を1つにまとめる" <|
                \run ->
                    Brainfuck.Operation.parse "++++"
                        |> Expect.equal (Ok [ VAdd 4 ])
            , test "連続する値操作を1つにまとめる" <|
                \run ->
                    Brainfuck.Operation.parse "+++-++-++----"
                        |> Expect.equal (Ok [ VAdd 1 ])
            , test "値操作で結局何もしない場合は命令ごと消し去る" <|
                \run ->
                    Brainfuck.Operation.parse "++--+-"
                        |> Expect.equal (Ok [])
            , test "連続する同方向へのポインタ操作を1つにまとめる" <|
                \run ->
                    Brainfuck.Operation.parse "<<<<"
                        |> Expect.equal (Ok [ PAdd -4 ])
            , test "連続するポインタ操作を1つにまとめる" <|
                \run ->
                    Brainfuck.Operation.parse "><>>>><>>><<<"
                        |> Expect.equal (Ok [ PAdd 3 ])
            , test "命令操作で結局何もしない場合は命令ごと消し去る" <|
                \run ->
                    Brainfuck.Operation.parse "<<><>>"
                        |> Expect.equal (Ok [])
            , test "値操作とポインタ操作が混ざってるバージョン" <|
                \run ->
                    Brainfuck.Operation.parse "++-<><+<>++<"
                        |> Expect.equal (Ok [ VAdd 1, PAdd -1, VAdd 1, VAdd 2, PAdd -1 ])
            , test "[-]はゼロクリアに置き換えられる" <|
                \run ->
                    Brainfuck.Operation.parse "[-]"
                        |> Expect.equal (Ok [ ZeroClear ])
            ]
        ]
