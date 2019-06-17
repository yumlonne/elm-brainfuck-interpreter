module Brainfuck.Operation exposing (Operation(..), parse)

import List.Extra

-- 愚直なパース結果
-- preparseで使う
type SimpleOperation
    = PI
    | PD
    | VI
    | VD
    | RD
    | PT
    | WL (List SimpleOperation)

-- +-,><と[-]は単純に最適化できる
type Operation
    = PAdd Int
    | VAdd Int
    | Read
    | Print
    | While (List Operation)
    | ZeroClear


parse : String -> Result String (List Operation)
parse program =
    preparse program
        |> Result.map optimize

optimize : List SimpleOperation -> List Operation
optimize ops =
    optimizeHelper ops []


optimizeHelper : List SimpleOperation -> List Operation -> List Operation
optimizeHelper ops optimized =
    case ops of
        [] ->
            List.reverse optimized

        op :: xs ->
            case op of
                WL [ VD ] ->
                    optimizeHelper xs <| ZeroClear :: optimized

                WL simpleOps ->
                    optimizeHelper xs <| (While <| optimize simpleOps) :: optimized

                PI ->
                    let
                        ( incs, other ) =
                            List.Extra.span (\x -> x == PI || x == PD) xs

                        incrementNum =
                            List.foldl (\x num -> num + toIncrementNum x) 1 incs

                        optimizedOperations =
                            if incrementNum == 0 then
                                optimized

                            else
                                PAdd incrementNum :: optimized
                    in
                    optimizeHelper other optimizedOperations

                PD ->
                    let
                        ( incs, other ) =
                            List.Extra.span (\x -> x == PI || x == PD) xs

                        incrementNum =
                            List.foldl (\x num -> num + toIncrementNum x) -1 incs

                        optimizedOperations =
                            if incrementNum == 0 then
                                optimized

                            else
                                PAdd incrementNum :: optimized
                    in
                    optimizeHelper other optimizedOperations

                VI ->
                    let
                        ( incs, other ) =
                            List.Extra.span (\x -> x == VI || x == VD) xs

                        incrementNum =
                            List.foldl (\x num -> num + toIncrementNum x) 1 incs

                        optimizedOperations =
                            if incrementNum == 0 then
                                optimized

                            else
                                VAdd incrementNum :: optimized
                    in
                    optimizeHelper other optimizedOperations

                VD ->
                    let
                        ( incs, other ) =
                            List.Extra.span (\x -> x == VI || x == VD) xs

                        incrementNum =
                            List.foldl (\x num -> num + toIncrementNum x) -1 incs

                        optimizedOperations =
                            if incrementNum == 0 then
                                optimized

                            else
                                VAdd incrementNum :: optimized
                    in
                    optimizeHelper other optimizedOperations

                RD ->
                    optimizeHelper xs (Read :: optimized)

                PT ->
                    optimizeHelper xs (Print :: optimized)


toIncrementNum : SimpleOperation -> Int
toIncrementNum op =
    case op of
        PI ->
            1

        PD ->
            -1

        VI ->
            1

        VD ->
            -1

        -- 少し嫌な感じがしつつ
        _ ->
            0

preparse : String -> Result String (List SimpleOperation)
preparse program =
    parseListChar (String.toList program) []


parseListChar : List Char -> List SimpleOperation -> Result String (List SimpleOperation)
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


parseWhile : List Char -> List SimpleOperation -> Result String ( SimpleOperation, List Char )
parseWhile chars ops =
    case ( chars, ops ) of
        -- 対応するカッコが見つからない
        ( [], _ ) ->
            Err "[]の対応がおかしいよ！"

        -- 対応するカッコまで来た
        ( ']' :: xs, res ) ->
            let
                op =
                    WL <| List.reverse res
            in
            Ok ( op, xs )

        -- ネストしてやがる
        ( '[' :: xs, _ ) ->
            let
                nestedWhileOp =
                    parseWhile xs []
            in
            -- map使って書き直したい
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


fromChar : Char -> Maybe SimpleOperation
fromChar c =
    case c of
        '>' ->
            Just PI

        '<' ->
            Just PD

        '+' ->
            Just VI

        '-' ->
            Just VD

        ',' ->
            Just RD

        '.' ->
            Just PT

        _ ->
            Nothing


