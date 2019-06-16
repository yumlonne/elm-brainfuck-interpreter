module Data.Byte exposing (Byte, decrement, fromInt, increment)


type alias Byte =
    Int


fromInt : Int -> Byte
fromInt =
    modBy 256


increment : Byte -> Byte
increment =
    fromInt << (+) 1


decrement : Byte -> Byte
decrement x =
    fromInt <| x - 1
