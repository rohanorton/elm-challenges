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
    , snake : Snake
    }


type alias Snake =
    { body : List Int
    , colour : String
    }


init : ( Model, Cmd Msg )
init =
    { board = 50
    , snake = Snake [ 5, 4, 3, 2, 1 ] "green"
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
            (List.map (cell model.snake) [0..model.board - 1])
        ]


cell : Snake -> Int -> Html Msg
cell snake index =
    let
        isSnakey =
            List.member index snake.body

        bgColour =
            if isSnakey then
                snake.colour
            else
                ""
    in
        td
            [ style
                [ "border" => "1px solid black"
                , "width" => "20px"
                , "height" => "20px"
                , "background-color" => bgColour
                ]
            ]
            []


(=>) : String -> String -> ( String, String )
(=>) =
    (,)
