module Data.Byte exposing (Byte, decrement, fromInt, increment, add)


type alias Byte =
    Int


fromInt : Int -> Byte
fromInt =
    modBy 256

add : Byte -> Int -> Byte
add x y = fromInt <| x + y

increment : Byte -> Byte
increment =
    fromInt << (+) 1


decrement : Byte -> Byte
decrement x =
    fromInt <| x - 1
