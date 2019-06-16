module Main exposing (main)

import Brainfuck
import Brainfuck.Operation as Operation
import Browser exposing (Document)
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)



-- MAIN


main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { brainfuck : Brainfuck.Model
    , program : String
    }


defaultArrayLength =
    1000


init : () -> ( Model, Cmd Msg )
init _ =
    ( { brainfuck = Brainfuck.init defaultArrayLength
      , program = ""
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = NoOp
    | OnInputProgram String
    | OnClickExecuteButton
    | OnInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        OnInputProgram program ->
            ( { model | program = program }
            , Cmd.none
            )

        OnClickExecuteButton ->
            let
                parsed =
                    Operation.parse model.program
            in
            case parsed of
                Ok ops ->
                    let
                        newBrainfuck =
                            Brainfuck.execAll ops model.brainfuck
                    in
                    ( { model | brainfuck = newBrainfuck }
                    , Cmd.none
                    )

                Err errMsg ->
                    ( model
                    , Cmd.none
                    )

        OnInput input ->
            let
                chars =
                    String.toList input

                newBrainfuck =
                    List.foldl Brainfuck.input model.brainfuck chars
            in
            ( { model | brainfuck = newBrainfuck }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "BrainF*ck"
    , body =
        [ layout [] <|
            column []
                [ el [] <|
                    Input.multiline []
                        { label = Input.labelAbove [] <| text "your program"
                        , onChange = OnInputProgram
                        , placeholder = Nothing
                        , spellcheck = False
                        , text = model.program
                        }
                , el [] <|
                    Input.button []
                        { label = text "実行"
                        , onPress = Just OnClickExecuteButton
                        }
                , el [] <|
                    let
                        labelText =
                            "プログラムへ入力する"

                        label =
                            if model.brainfuck.waitingInput then
                                labelText ++ " !!"

                            else
                                labelText
                    in
                    Input.text []
                        { onChange = OnInput
                        , text = ""
                        , placeholder = Nothing
                        , label = Input.labelLeft [] <| text label
                        }
                , el [] <|
                    Input.multiline []
                        { label = Input.labelAbove [] <| text "result"
                        , onChange = always NoOp
                        , placeholder = Nothing
                        , spellcheck = False
                        , text = model.brainfuck.outputString
                        }
                ]
        ]
    }
