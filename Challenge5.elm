module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App as Html


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { board : Int
    }


init : ( Model, Cmd Msg )
init =
    { board = 50
    }
        ! []



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    table
        [ style
            [ "border" => "1px solid black"
            , "border-collapse" => "collapse"
            , "margin" => "auto"
            , "margin-top" => "100px"
            ]
        ]
        [ tr []
            (List.repeat model.board cell)
        ]


cell : Html Msg
cell =
    td
        [ style
            [ "border" => "1px solid black"
            , "width" => "20px"
            , "height" => "20px"
            ]
        ]
        []


(=>) : String -> String -> ( String, String )
(=>) =
    (,)
