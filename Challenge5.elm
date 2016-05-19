module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Time exposing (millisecond, Time)
import Keyboard exposing (KeyCode)
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
    , apple : Apple
    , score : Int
    , leftToGrow : Int
    , grows : Int
    }


type alias Snake =
    { body : List Position
    , colour : String
    , direction : Direction
    , nextDirection : Direction
    }


type alias Apple =
    { position : Position
    , colour : String
    }


type Direction
    = Up
    | Right
    | Down
    | Left


type alias Position =
    { x : Int, y : Int }


init : ( Model, Cmd Msg )
init =
    { board = 20
    , snake = Snake [ { x = 5, y = 5 }, { x = 4, y = 5 }, { x = 3, y = 5 }, { x = 2, y = 5 }, { x = 1, y = 5 } ] "green" Right Right
    , apple = Apple { x = 9, y = 5 } "red"
    , score = 0
    , leftToGrow = 0
    , grows = 3
    }
        ! []



-- Update


type Msg
    = Move
    | OnKey KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ snake, apple, score } as model) =
    case msg of
        OnKey key ->
            case key of
                37 ->
                    { model | snake = turn Left snake } ! []

                38 ->
                    { model | snake = turn Up snake } ! []

                39 ->
                    { model | snake = turn Right snake } ! []

                40 ->
                    { model | snake = turn Down snake } ! []

                _ ->
                    model ! []

        Move ->
            let
                newHead =
                    List.head snake.body
                        |> Maybe.withDefault { x = 0, y = 0 }
                        |> inc direction'
                        |> wrap model

                ( snakeInit, snakeLast ) =
                    (initAndLast snake.body)

                newTail =
                    if model.leftToGrow > 0 then
                        snake.body
                    else
                        snakeInit

                body' =
                    newHead :: newTail

                direction' =
                    snake.nextDirection

                snake' =
                    { snake | body = body', direction = direction' }

                ( score', apple', leftToGrow' ) =
                    if newHead == apple.position then
                        ( score + 1
                        , { apple | position = snakeLast }
                        , model.grows
                        )
                    else
                        ( score, apple, model.leftToGrow - 1 )

                collision =
                    List.member newHead newTail
            in
                if collision then
                    init
                else
                    { model
                        | snake = snake'
                        , apple = apple'
                        , score = score'
                        , leftToGrow = leftToGrow'
                    }
                        ! []


movingHorizontally : Snake -> Bool
movingHorizontally snake =
    snake.direction == Right || snake.direction == Left


turn : Direction -> Snake -> Snake
turn direction snake =
    if turnCondition direction snake then
        { snake | nextDirection = direction }
    else
        snake


turnCondition : Direction -> Snake -> Bool
turnCondition direction snake =
    case direction of
        Left ->
            not (movingHorizontally snake)

        Right ->
            not (movingHorizontally snake)

        _ ->
            movingHorizontally snake


inc : Direction -> Position -> Position
inc direction { x, y } =
    case direction of
        Right ->
            { x = x + 1, y = y }

        Left ->
            { x = x - 1, y = y }

        Up ->
            { x = x, y = y - 1 }

        Down ->
            { x = x, y = y + 1 }


wrap : Model -> Position -> Position
wrap { board } { x, y } =
    { x = x % board, y = y % board }



{- both init and last functions are O(n) already, lets not do twice the work
   required!

    TODO: There is clearly a runtime bug in here. Need to change snake to have
    a non-empty list type.
-}


initAndLast : List a -> ( List a, a )
initAndLast list =
    case list of
        [] ->
            Debug.crash "Cannot use empty list"

        x :: [] ->
            ( [], x )

        x :: y :: [] ->
            ( [ x ], y )

        x :: xs ->
            let
                ( init, last ) =
                    initAndLast xs
            in
                ( x :: init, last )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (100 * millisecond) (always Move)
        , Keyboard.downs OnKey
        ]



-- View


view : Model -> Html Msg
view ({ score } as model) =
    div
        [ style
            [ "text-align" => "center"
            ]
        ]
        [ h1
            [ style
                [ "padding" => "20px"
                , "font-family" => "monospace"
                ]
            ]
            [ text <| "Score: " ++ toString score ]
        , table
            [ style
                [ "border" => "2px solid black"
                , "border-collapse" => "collapse"
                , "margin" => "auto"
                ]
            ]
            (List.map (row model) [0..model.board - 1])
        ]


row : Model -> Int -> Html Msg
row model yIndex =
    tr []
        (List.map (cell model yIndex) [0..model.board - 1])


cell : Model -> Int -> Int -> Html Msg
cell { snake, apple } yIndex xIndex =
    let
        position =
            { x = xIndex, y = yIndex }

        isSnakey =
            List.member position snake.body

        isAppley =
            position == apple.position

        bgColour =
            if isSnakey then
                snake.colour
            else if isAppley then
                apple.colour
            else
                ""
    in
        td
            [ style
                [ "width" => "20px"
                , "height" => "20px"
                , "background-color" => bgColour
                ]
            ]
            []


(=>) : String -> String -> ( String, String )
(=>) =
    (,)
