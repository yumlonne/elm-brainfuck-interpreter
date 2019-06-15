module Test.Data.Byte exposing (suite)

import Data.Byte
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Data.Byte module"
        [ describe "fromInt"
            [ test "0から255までの値には何もしない" <|
                \run ->
                    let
                        data =
                            List.range 0 255
                    in
                    data
                        |> List.map Data.Byte.fromInt
                        |> Expect.equal data
            , test "256以上の値は256で割ったあまりにする" <|
                \run ->
                    let
                        data =
                            [ 256, 280, 1025 ]
                    in
                    data
                        |> List.map Data.Byte.fromInt
                        |> Expect.equal [ 0, 24, 1 ]
            , test "負数は無いので-1 -> 255のように変換する" <|
                \run ->
                    let
                        data =
                            [-1, -200, -255, -256, -257]
                    in
                    data
                        |> List.map Data.Byte.fromInt
                        |> Expect.equal [255, 56, 1, 0, 255]
            ]
        , describe "increment"
            [ test "Byte値に1を足す" <|
                \run ->
                    List.range 0 254
                        |> List.map (Data.Byte.fromInt >> Data.Byte.increment)
                        |> Expect.equal (List.range 1 255)
            , test "overflow" <|
                \run ->
                    255
                        |> Data.Byte.fromInt >> Data.Byte.increment
                        |> Expect.equal 0
            ]
        , describe "decrement"
            [ test "Byte値から1を引く" <|
                \run ->
                    List.range 1 255
                        |> List.map (Data.Byte.fromInt >> Data.Byte.decrement)
                        |> Expect.equal (List.range 0 254)
            , test "overflow" <|
                \run ->
                    0
                        |> Data.Byte.fromInt >> Data.Byte.decrement
                        |> Expect.equal 255
            ]
        ]
