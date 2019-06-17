module Test.Brainfuck exposing (suite)

import Array
import Brainfuck
import Brainfuck.Operation as Operation exposing (Operation(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Queue
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
                        |> Expect.equal (Array.fromList [ 0, 0, 0, 0, 0 ])
            ]
        , describe "exec, execAll"
            [ test "ポインタ、メモリに対する操作を実行できる" <|
                \run ->
                    let
                        model =
                            Brainfuck.init 3

                        ops =
                            [ VAdd 1, PAdd 1, VAdd 1, PAdd 1 ]
                    in
                    model
                        |> Brainfuck.execAll ops
                        |> (\m ->
                                ( m.memory, m.pointer )
                                    |> Expect.equal ( Array.fromList [ 1, 1, 0 ], 2 )
                           )
            , describe "Readの実行できる" <|
                let
                    modelForRead =
                        Brainfuck.init 1
                in
                [ test "実行時にinputQueueに文字がなかった場合" <|
                    \run ->
                        modelForRead
                            |> Brainfuck.exec Read
                            |> Expect.equal
                                { modelForRead
                                    | waitingInput = True
                                    , operationQueue = Queue.fromList [ Read ]
                                }
                , test "実行時にinputQueueに文字があった場合" <|
                    \run ->
                        let
                            inputChar =
                                'X'

                            inputtedModel =
                                { modelForRead | inputQueue = Queue.fromList [ inputChar ] }
                        in
                        inputtedModel
                            |> Brainfuck.exec Read
                            |> Expect.equal
                                { modelForRead
                                    | memory = Array.fromList [ Char.toCode inputChar ]
                                }
                ]
            , test "Printの実行ができる" <|
                \run ->
                    let
                        expectChar =
                            'Z'

                        tmpModel =
                            Brainfuck.init 1

                        model =
                            { tmpModel | memory = Array.fromList [ Char.toCode expectChar ] }
                    in
                    model
                        |> Brainfuck.exec Print
                        |> Expect.equal
                            { model
                                | outputString = String.fromChar expectChar
                            }
            , describe "Whileの実行ができる" <|
                let
                    modelForWhile =
                        Brainfuck.init 5
                in
                [ describe "条件分岐ができる"
                    [ test "0ならまるまるスキップ" <|
                        \run ->
                            modelForWhile
                                |> Brainfuck.exec (While [ VAdd 1 ])
                                |> Expect.equal modelForWhile
                    , test "0以外なら実行" <|
                        \run ->
                            modelForWhile
                                |> Brainfuck.execAll [ VAdd 1, While [ PAdd 1, VAdd 1, PAdd -1, VAdd -1 ] ]
                                |> Expect.equal
                                    { modelForWhile
                                        | memory = Array.fromList [ 0, 1, 0, 0, 0 ]
                                    }
                    ]
                , describe "繰り返しができる"
                    [ test "足し算" <|
                        \run ->
                            modelForWhile
                                -- 繰り返しによる足し算(1 + 2)
                                -- https://tondol.hatenablog.jp/entry/20100630/1277839735
                                |> Brainfuck.execAll [ VAdd 1, PAdd 1, VAdd 2, PAdd -1 ]
                                |> Brainfuck.execAll [ PAdd 1, While [ VAdd -1, PAdd -1, VAdd 1, PAdd 1 ], PAdd -1 ]
                                |> Expect.equal
                                    { modelForWhile
                                        | memory = Array.fromList [ 3, 0, 0, 0, 0 ]
                                    }
                    , test "内部でread待ちになっても正しく動く" <|
                        \run ->
                            modelForWhile
                                -- While発火用のVInc
                                |> Brainfuck.exec (VAdd 1)
                                |> Brainfuck.exec (While [ Read, PAdd 1, VAdd 1 ])
                                |> (\m ->
                                        ( m.waitingInput, Queue.toList m.operationQueue )
                                            |> Expect.equal
                                                ( True
                                                , [ Read, PAdd 1, VAdd 1, While [ Read, PAdd 1, VAdd 1 ] ]
                                                )
                                   )
                    ]
                ]
            ]
        , describe "optimized check" <|
            let
                modelForExecOptimized =
                    Brainfuck.init 2
            in
            [ test "PAdd n はポインタにnを加える" <|
                \run ->
                    modelForExecOptimized
                        |> Brainfuck.exec (PAdd 10)
                        |> Expect.equal
                            { modelForExecOptimized
                                | pointer = 10
                            }
            , test "VAdd n はフォーカスしているメモリにnを加える" <|
                \run ->
                    modelForExecOptimized
                        |> Brainfuck.exec (VAdd 17)
                        |> Expect.equal
                            { modelForExecOptimized
                                | memory = Array.fromList [ 17, 0 ]
                            }
            , test "ZeroClearはフォーカスしているメモリをゼロクリアする" <|
                \run ->
                    modelForExecOptimized
                        |> Brainfuck.exec (VAdd 255)
                        |> Brainfuck.exec ZeroClear
                        |> Expect.equal modelForExecOptimized
            ]
        , describe "input" <|
            let
                modelForInput =
                    Brainfuck.init 2
            in
            [ test "read待ちでなければinputQueueに追加する" <|
                \run ->
                    modelForInput
                        |> Brainfuck.input 'a'
                        |> .inputQueue
                        |> Queue.toList
                        |> Expect.equal [ 'a' ]
            , describe "read待ちならプログラムが進行する"
                [ test "普通のread待ち" <|
                    \run ->
                        modelForInput
                            |> Brainfuck.execAll [ Read, PAdd 1, VAdd 1 ]
                            |> Brainfuck.input 'a'
                            |> Expect.equal
                                { modelForInput
                                    | memory = Array.fromList [ Char.toCode 'a', 1 ]
                                    , pointer = 1
                                }
                , test "While内でread待ち" <|
                    \run ->
                        modelForInput
                            |> Brainfuck.execAll [ VAdd 1, While [ PAdd 1, Read, PAdd -1, VAdd -1 ], Read ]
                            |> Brainfuck.input 'A'
                            |> Brainfuck.input 'Z'
                            |> Expect.equal
                                { modelForInput
                                    | memory = Array.fromList [ Char.toCode 'Z', Char.toCode 'A' ]
                                }
                ]
            ]
        ]
