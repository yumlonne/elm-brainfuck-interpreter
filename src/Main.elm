module Main exposing (main)

import Browser

-- MAIN

main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }




init : () -> ( (), Cmd () )
init = Debug.todo "impl"
subscriptions = Debug.todo "impl"
update = Debug.todo "impl"
view = Debug.todo "impl"
