module Brainfuck.Operation exposing (..)

type Operation
    = PInc
    | PDec
    | VInc
    | VDec
    | Read
    | Print
    | While (List Operation)


parse : String -> Result String (List (Operation))
parse = Debug.todo ""

parseListChar : List Char -> List (Operation) -> Result String (List (Operation))
parseListChar chars ops =
    case chars of
        [] ->
            Ok <| List.reverse ops

        '[' :: xs ->
            let
                result = parseWhile xs []
            in
            case result of
                Ok (op, taked) ->
                    parseListChar taked (op :: ops)

                -- TODO: 何文字目とか出してもいいかも
                Err msg ->
                    Err msg

        -- FIXME: parseWhileに同じのが出てるので共通化したい
        ']' :: xs ->
            Err "[]の対応がおかしいよ！"

        c :: xs ->
            let
                maybeOp = fromChar c
            in
            case maybeOp of
                Just op ->
                    parseListChar xs (op :: ops)
                Nothing ->
                    Debug.todo "todo '[' ? "

parseWhile : List Char -> List Operation -> Result String (Operation, (List Char))
parseWhile chars ops =
    case (chars, ops) of
        -- 対応するカッコが見つからない
        ([], _) ->
            Err "[]の対応がおかしいよ！"


        -- 対応するカッコまで来た
        (']' :: xs, res) ->
            let
                op = While <| List.reverse res
            in
            Ok (op, xs)


        -- ネストしてやがる
        ('[' :: xs, _) ->
            let
                nestedWhileOp = parseWhile xs []
            in
            case nestedWhileOp of
                Ok (op, taked) ->
                    parseWhile taked (op :: ops)

                Err msg ->
                    Err msg


        -- その他普通のOperationまたは無効文字
        (char :: xs, _) ->
            let
                maybeOp = fromChar char
            in
            case maybeOp of
                Just op ->
                    parseWhile xs (op :: ops)
                Nothing ->
                    parseWhile xs ops




fromChar : Char -> Maybe Operation
fromChar c =
    case c of
        '>' -> Just PInc
        '<' -> Just PDec
        '+' -> Just VInc
        '-' -> Just VDec
        ',' -> Just Read
        '.' -> Just Print
        _ -> Nothing
