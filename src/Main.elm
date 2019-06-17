module Main exposing (main)

import Brainfuck
import Brainfuck.Operation as Operation
import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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
    , inputLog : String
    , errorMsg : String
    }


defaultArrayLength =
    1000


init : () -> ( Model, Cmd Msg )
init _ =
    ( { brainfuck = Brainfuck.init defaultArrayLength
      , program = ""
      , inputLog = ""
      , errorMsg = ""
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
                    Debug.log "parsed program" <| Operation.parse model.program
            in
            case parsed of
                Ok ops ->
                    let
                        newBrainfuck =
                            Brainfuck.execAll ops model.brainfuck
                    in
                    ( { model | brainfuck = newBrainfuck, errorMsg = "" }
                    , Cmd.none
                    )

                Err errorMsg ->
                    ( { model | errorMsg = errorMsg }
                    , Cmd.none
                    )

        OnInput input ->
            let
                chars =
                    String.toList input

                newBrainfuck =
                    List.foldl Brainfuck.input model.brainfuck chars
            in
            ( { model | brainfuck = newBrainfuck, inputLog = model.inputLog ++ input }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "BrainF*ck"
    , body =
        [ layout [] <|
            row [ width fill, height fill, padding 30 ]
                [ column [ centerX, width <| px 600, spacing 40 ]
                    [ el [ centerX ] <|
                        Input.multiline [ centerX, width <| px 400, height <| px 480, Font.size 15 ]
                            { label = Input.labelAbove [ centerX ] <| text "↓ここにBrainf*ckプログラムを入力する"
                            , onChange = OnInputProgram
                            , placeholder = Nothing
                            , spellcheck = False
                            , text = model.program
                            }
                    , row [ alignRight, paddingXY 100 0 ]
                        [ el [ paddingXY 50 0, Font.color <| rgb255 228 64 64 ] <|
                            text model.errorMsg
                        , el [] <|
                            Input.button [ Background.color <| rgb255 128 128 228, width <| px 80, height <| px 50, centerX, centerY ]
                                { label = el [ centerX ] <| text "実行"
                                , onPress = Just OnClickExecuteButton
                                }
                        ]
                    , row [ centerX, height <| px 40 ]
                        [ el [ centerX, height <| px 40 ] <|
                            let
                                labelText =
                                    "入力"

                                label =
                                    if model.brainfuck.waitingInput then
                                        labelText ++ " (待機中...)"

                                    else
                                        labelText
                            in
                            Input.text [ height <| px 40 ]
                                { onChange = OnInput
                                , text = ""
                                , placeholder = Nothing
                                , label = Input.labelLeft [ centerY ] <| text label
                                }
                        , el [ centerY, padding 10 ] <|
                            Input.button [ width <| px 25, height <| px 40, Background.color <| rgb255 192 192 192 ]
                                { label = el [ centerX, Font.color <| rgb255 0 0 0 ] <| text "\\0"
                                , onPress = Just << OnInput << String.fromChar <| Char.fromCode 0
                                }
                        ]
                    ]
                , el [ centerX ] <|
                    Input.multiline [ centerX, width <| px 500, height <| px 650, padding 30, Font.size 15 ]
                        { label = Input.labelAbove [ centerX ] <| text "出力"
                        , onChange = always NoOp
                        , placeholder = Nothing
                        , spellcheck = False
                        , text = model.brainfuck.outputString
                        }
                ]
        ]
    }
