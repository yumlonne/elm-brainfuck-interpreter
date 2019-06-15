module Brainfuck exposing (Model, exec)

import Array exposing (Array)
import Brainfuck.Operation as Operation exposing (Operation(..))
import Data.Byte as Byte exposing (Byte)
import Queue exposing (Queue)


type alias Model =
    { memory : Array Byte
    , pointer : Int
    , operationQueue : Queue Operation
    , waitingInput : Bool
    , inputQueue : Queue Char
    , outputString : String
    }


exec : Operation -> Model -> Model
exec op model =
    if model.waitingInput then
        { model | operationQueue = Queue.enqueue op model.operationQueue }

    else
        case op of
            PInc ->
                { model | pointer = model.pointer + 1 }

            PDec ->
                -- XXX: 負数になっちゃうかもだけどそれはプログラムのミス
                { model | pointer = model.pointer - 1 }

            VInc ->
                setMem (Maybe.withDefault 0 <| Maybe.map Byte.increment <| getMem model) model

            VDec ->
                setMem (Maybe.withDefault 0 <| Maybe.map Byte.decrement <| getMem model) model

            Read ->
                case Queue.dequeue model.inputQueue of
                    ( Just v, queue ) ->
                        let
                            updatedModel =
                                setMem (Byte.fromInt << Char.toCode <| v) model

                            newModel =
                                { updatedModel | inputQueue = queue }
                        in
                        newModel

                    ( Nothing, _ ) ->
                        { model
                            | waitingInput = True
                            , operationQueue = Queue.enqueue op model.operationQueue
                        }

            Print ->
                let
                    str =
                        getMem model
                            |> Maybe.withDefault 0
                            |> Char.fromCode
                            |> String.fromChar
                in
                { model | outputString = model.outputString ++ str }

            While ops ->
                Debug.todo "impl"


getMem : Model -> Maybe Byte
getMem model =
    Array.get model.pointer model.memory


setMem : Byte -> Model -> Model
setMem v model =
    { model | memory = Array.set model.pointer v model.memory }
