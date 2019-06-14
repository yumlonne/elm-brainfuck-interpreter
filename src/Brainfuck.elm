module Brainfuck exposing (..)

import Array exposing(Array)

type alias Model =
    { memory: Array String
    , pointer: Int
    }

