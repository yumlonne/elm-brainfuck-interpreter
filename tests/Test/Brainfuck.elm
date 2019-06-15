module Test.Brainfuck exposing (..)

import Brainfuck
import Brainfuck.Operation as Operation exposing (Operation(..))
import Array
import Queue
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
    describe "The Brainfuck module"
        [ describe "init"
            [ test "指定した長さの配列を作る" <|
                \run ->
                    Brainfuck.init 10
                        |> .memory
                        |> Array.length
                        |> Expect.equal 10
            , test "0クリアされている" <|
                \run ->
                    Brainfuck.init 5
                        |> .memory
                        |> Expect.equal (Array.fromList [0, 0, 0, 0, 0])
            ]
        , describe "exec"
            [ test "ポインタ、メモリに対する操作を実行できる" <|
                \run ->
                    let
                        model = Brainfuck.init 3
                        ops = [VInc, PInc, VInc, VInc, VDec, PInc, PInc, PDec]
                    in
                    model
                        |> Brainfuck.execAll ops
                        |> \m -> (m.memory, m.pointer)
                        |> Expect.equal (Array.fromList [1, 1, 0], 2)
            , describe "Readの実行できる" <|
                let
                    modelForRead = Brainfuck.init 1
                in
                [ test "実行時にinputQueueに文字がなかった場合" <|
                    \run ->
                        modelForRead
                            |> Brainfuck.exec Read
                            |> Expect.equal
                                { modelForRead
                                    | waitingInput = True
                                    , operationQueue = Queue.fromList [Read]
                                }
                , test "実行時にinputQueueに文字があった場合" <|
                    \run ->
                        let
                            inputChar = 'X'
                            inputtedModel = { modelForRead | inputQueue = Queue.fromList [inputChar] }
                        in
                        inputtedModel
                            |> Brainfuck.exec Read
                            |> Expect.equal
                                { modelForRead
                                    | memory = Array.fromList [Char.toCode inputChar]
                                }
                ]
            , test "Printの実行ができる" <|
                \run ->
                    let
                        expectChar = 'Z'
                        tmpModel = Brainfuck.init 1
                        model = { tmpModel | memory = Array.fromList [Char.toCode expectChar] }
                    in
                    model
                        |> Brainfuck.exec Print
                        |> Expect.equal
                            { model
                                | outputString = String.fromChar expectChar
                            }
            , describe "Whileの実行ができる" <|
                let
                    modelForWhile = Brainfuck.init 5
                in
                [ describe "条件分岐ができる"
                    [ test "0ならまるまるスキップ" <|
                        \run ->
                            modelForWhile
                                |> Brainfuck.exec (While [VInc])
                                |> Expect.equal modelForWhile
                    , test "0以外なら実行" <|
                        \run ->
                            modelForWhile
                                |> Brainfuck.execAll [VInc, While [PInc, VInc, PDec, VDec]]
                                |> Expect.equal
                                    { modelForWhile
                                        | memory = Array.fromList [0, 1, 0, 0, 0]
                                    }
                    ]
                , describe "繰り返しができる"
                    [ test "足し算" <|
                        \run ->
                            modelForWhile
                                -- 繰り返しによる足し算(1 + 2)
                                -- https://tondol.hatenablog.jp/entry/20100630/1277839735
                                |> Brainfuck.execAll [VInc, PInc, VInc, VInc, PDec]
                                |> Brainfuck.execAll [PInc, While [VDec, PDec, VInc, PInc], PDec]
                                |> Expect.equal
                                    { modelForWhile
                                        | memory = Array.fromList [3, 0, 0, 0, 0]
                                    }
                    , test "内部でread待ちになっても正しく動く" <|
                        \run ->
                            modelForWhile
                                -- While発火用のVInc
                                |> Brainfuck.exec VInc
                                |> Brainfuck.exec (While [Read, PInc, VInc])
                                |> \m -> (m.waitingInput, Queue.toList m.operationQueue)
                                |> Expect.equal
                                    ( True
                                    , [Read, PInc, VInc, While [Read, PInc, VInc]]
                                    )
                    ]
                ]
            ]

        ]
