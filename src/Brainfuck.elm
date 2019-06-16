module Brainfuck exposing (Model, exec, execAll, init, input)

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


init : Int -> Model
init arraySize =
    { memory = Array.repeat arraySize 0
    , pointer = 0
    , operationQueue = Queue.empty
    , waitingInput = False
    , inputQueue = Queue.empty
    , outputString = ""
    }


input : Char -> Model -> Model
input char model =
    if model.waitingInput then
        execAll
            (Queue.toList model.operationQueue)
            { model
                | inputQueue = Queue.enqueue char model.inputQueue
                , operationQueue = Queue.fromList []
                , waitingInput = False
            }

    else
        { model
            | inputQueue = Queue.enqueue char model.inputQueue
            , waitingInput = False
        }


execAll : List Operation -> Model -> Model
execAll ops model =
    List.foldl exec model ops


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
                if getWhileCond model then
                    let
                        appliedModel =
                            execAll ops model
                    in
                    -- Whileの中でread待ちになったらoperationQueueにWhile本体が積まれないが、ここで再実行すればoperationQueueに積まれる
                    -- 待ちになっていなくても再実行することで条件の再判定ごと行われるため繰り返し処理となる
                    exec op appliedModel

                else
                    model


getMem : Model -> Maybe Byte
getMem model =
    Array.get model.pointer model.memory


setMem : Byte -> Model -> Model
setMem v model =
    { model | memory = Array.set model.pointer v model.memory }


getWhileCond : Model -> Bool
getWhileCond model =
    getMem model
        |> Maybe.map (\v -> v /= 0)
        |> Maybe.withDefault False
