module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Time exposing (millisecond, Time)
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
    = Move


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ snake } as model) =
    case msg of
        Move ->
            let
                newHead =
                    List.head snake.body
                        |> Maybe.withDefault 0
                        |> (+) 1

                body' =
                    newHead :: (first snake.body)

                snake' =
                    { snake | body = body' }
            in
                { model | snake = snake' } ! []


first : List a -> List a
first list =
    case list of
        [] ->
            []

        x :: [] ->
            []

        x :: y :: [] ->
            [ x ]

        x :: xs ->
            x :: first xs



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (100 * millisecond) (always Move)



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
