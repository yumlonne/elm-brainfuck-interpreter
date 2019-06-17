module Test.Brainfuck.Operation exposing (suite)

import Brainfuck.Operation exposing (Operation(..), OptimizedOperation(..))
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
                        |> Expect.equal (Ok [ VInc, VDec, VInc, VInc, PDec, Print, Read, Print, PInc ])
            , test "whileループが含まれていてもOK" <|
                \run ->
                    let
                        program =
                            "[+]--<"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [ While [ VInc ], VDec, VDec, PDec ])
            , test "ネストループが含まれていてもOK" <|
                \run ->
                    let
                        program =
                            "+[[>]<+>[[+]]]"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [ VInc, While [ While [ PInc ], PDec, VInc, PInc, While [ While [ VInc ] ] ] ])
            , test "特定の記号以外は無視する" <|
                \run ->
                    let
                        program =
                            "hoge+fuga<?-"
                    in
                    Brainfuck.Operation.parse program
                        |> Expect.equal (Ok [ VInc, PDec, VDec ])
            ]
        , describe "optimizedParse"
            [ test "連続する同方向への値操作を1つにまとめる" <|
                \run ->
                    Brainfuck.Operation.optimizedParse "++++"
                        |> Expect.equal (Ok [ OptimizedValueAdd 4 ])
            , test "連続する値操作を1つにまとめる" <|
                \run ->
                    Brainfuck.Operation.optimizedParse "+++-++-++----"
                        |> Expect.equal (Ok [ OptimizedValueAdd 1 ])
            , test "値操作で結局何もしない場合は命令ごと消し去る" <|
                \run ->
                    Brainfuck.Operation.optimizedParse "++--+-"
                        |> Expect.equal (Ok [])
            , test "連続する同方向へのポインタ操作を1つにまとめる" <|
                \run ->
                    Brainfuck.Operation.optimizedParse "<<<<"
                        |> Expect.equal (Ok [ OptimizedPointerAdd -4 ])
            , test "連続するポインタ操作を1つにまとめる" <|
                \run ->
                    Brainfuck.Operation.optimizedParse "><>>>><>>><<<"
                        |> Expect.equal (Ok [ OptimizedPointerAdd 3 ])
            , test "命令操作で結局何もしない場合は命令ごと消し去る" <|
                \run ->
                    Brainfuck.Operation.optimizedParse "<<><>>"
                        |> Expect.equal (Ok [])
            , test "値操作とポインタ操作が混ざってるバージョン" <|
                \run ->
                    Brainfuck.Operation.optimizedParse "++-<><+<>++<"
                        |> Expect.equal (Ok [ OptimizedValueAdd 1, OptimizedPointerAdd -1, OptimizedValueAdd 1, OptimizedValueAdd 2, OptimizedPointerAdd -1 ])
            , test "[-]はゼロクリアに置き換えられる" <|
                \run ->
                    Brainfuck.Operation.optimizedParse "[-]"
                        |> Expect.equal (Ok [ OptimizedZeroClear ])
            ]
        ]
