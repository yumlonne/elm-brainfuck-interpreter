module Brainfuck.Operation exposing (Operation(..), OptimizedOperation(..), optimizedParse, parse)

import List.Extra


type Operation
    = PInc
    | PDec
    | VInc
    | VDec
    | Read
    | Print
    | While (List Operation)



-- +-,><と[-]は最適化できる


type OptimizedOperation
    = OptimizedPointerAdd Int
    | OptimizedValueAdd Int
    | OptimizedZeroClear
    | OptimizedWhile (List OptimizedOperation)
    | BasicOperation Operation


parse : String -> Result String (List Operation)
parse program =
    parseListChar (String.toList program) []


optimizedParse : String -> Result String (List OptimizedOperation)
optimizedParse program =
    parse program
        |> Result.map optimize


parseListChar : List Char -> List Operation -> Result String (List Operation)
parseListChar chars ops =
    case chars of
        [] ->
            Ok <| List.reverse ops

        '[' :: xs ->
            let
                result =
                    parseWhile xs []
            in
            case result of
                Ok ( op, taked ) ->
                    parseListChar taked (op :: ops)

                -- TODO: 何文字目とか出してもいいかも
                Err msg ->
                    Err msg

        -- FIXME: parseWhileに同じのが出てるので共通化したい
        ']' :: xs ->
            Err "[]の対応がおかしいよ！"

        c :: xs ->
            let
                maybeOp =
                    fromChar c
            in
            case maybeOp of
                Just op ->
                    parseListChar xs (op :: ops)

                Nothing ->
                    parseListChar xs ops


parseWhile : List Char -> List Operation -> Result String ( Operation, List Char )
parseWhile chars ops =
    case ( chars, ops ) of
        -- 対応するカッコが見つからない
        ( [], _ ) ->
            Err "[]の対応がおかしいよ！"

        -- 対応するカッコまで来た
        ( ']' :: xs, res ) ->
            let
                op =
                    While <| List.reverse res
            in
            Ok ( op, xs )

        -- ネストしてやがる
        ( '[' :: xs, _ ) ->
            let
                nestedWhileOp =
                    parseWhile xs []
            in
            case nestedWhileOp of
                Ok ( op, taked ) ->
                    parseWhile taked (op :: ops)

                Err msg ->
                    Err msg

        -- その他普通のOperationまたは無効文字
        ( char :: xs, _ ) ->
            let
                maybeOp =
                    fromChar char
            in
            case maybeOp of
                Just op ->
                    parseWhile xs (op :: ops)

                Nothing ->
                    parseWhile xs ops


fromChar : Char -> Maybe Operation
fromChar c =
    case c of
        '>' ->
            Just PInc

        '<' ->
            Just PDec

        '+' ->
            Just VInc

        '-' ->
            Just VDec

        ',' ->
            Just Read

        '.' ->
            Just Print

        _ ->
            Nothing


optimize : List Operation -> List OptimizedOperation
optimize ops =
    optimizeHelper ops []


optimizeHelper : List Operation -> List OptimizedOperation -> List OptimizedOperation
optimizeHelper ops optimized =
    case ops of
        [] ->
            List.reverse optimized

        op :: xs ->
            case op of
                While [ VDec ] ->
                    optimizeHelper xs <| OptimizedZeroClear :: optimized

                While basicOps ->
                    optimizeHelper xs <| (OptimizedWhile <| optimize basicOps) :: optimized

                PInc ->
                    let
                        ( incs, other ) =
                            List.Extra.span (\x -> x == PInc || x == PDec) xs

                        incrementNum =
                            List.foldl (\x num -> num + toIncrementNum x) 1 incs

                        optimizedOperations =
                            if incrementNum == 0 then
                                optimized

                            else
                                OptimizedPointerAdd incrementNum :: optimized
                    in
                    optimizeHelper other optimizedOperations

                PDec ->
                    let
                        ( incs, other ) =
                            List.Extra.span (\x -> x == PInc || x == PDec) xs

                        incrementNum =
                            List.foldl (\x num -> num + toIncrementNum x) -1 incs

                        optimizedOperations =
                            if incrementNum == 0 then
                                optimized

                            else
                                OptimizedPointerAdd incrementNum :: optimized
                    in
                    optimizeHelper other optimizedOperations

                VInc ->
                    let
                        ( incs, other ) =
                            List.Extra.span (\x -> x == VInc || x == VDec) xs

                        incrementNum =
                            List.foldl (\x num -> num + toIncrementNum x) 1 incs

                        optimizedOperations =
                            if incrementNum == 0 then
                                optimized

                            else
                                OptimizedValueAdd incrementNum :: optimized
                    in
                    optimizeHelper other optimizedOperations

                VDec ->
                    let
                        ( incs, other ) =
                            List.Extra.span (\x -> x == VInc || x == VDec) xs

                        incrementNum =
                            List.foldl (\x num -> num + toIncrementNum x) -1 incs

                        optimizedOperations =
                            if incrementNum == 0 then
                                optimized

                            else
                                OptimizedValueAdd incrementNum :: optimized
                    in
                    optimizeHelper other optimizedOperations

                _ ->
                    optimizeHelper xs <| BasicOperation op :: optimized


toIncrementNum : Operation -> Int
toIncrementNum op =
    case op of
        PInc ->
            1

        PDec ->
            -1

        VInc ->
            1

        VDec ->
            -1

        -- 少し嫌な感じがしつつ
        _ ->
            0
