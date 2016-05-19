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
    , apple : Apple
    , score : Int
    , leftToGrow : Int
    , grows : Int
    }


type alias Snake =
    { body : List Int
    , colour : String
    }


type alias Apple =
    { position : Int
    , colour : String
    }


init : ( Model, Cmd Msg )
init =
    { board = 50
    , snake = Snake [ 5, 4, 3, 2, 1 ] "green"
    , apple = Apple 9 "red"
    , score = 0
    , leftToGrow = 0
    , grows = 3
    }
        ! []



-- Update


type Msg
    = Move


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ snake, apple, score } as model) =
    case msg of
        Move ->
            let
                newHead =
                    List.head snake.body
                        |> Maybe.withDefault 0
                        |> (+) 1
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

                snake' =
                    { snake | body = body' }

                ( score', apple', leftToGrow' ) =
                    if newHead == apple.position then
                        ( score + 1
                        , { apple | position = snakeLast }
                        , model.grows
                        )
                    else
                        ( score, apple, model.leftToGrow - 1 )
            in
                { model
                    | snake = snake'
                    , apple = apple'
                    , score = score'
                    , leftToGrow = leftToGrow'
                }
                    ! []


wrap : Model -> Int -> Int
wrap { board } newHead =
    if board > newHead then
        newHead
    else
        0



{- both init and last functions are O(n) already, lets not do twice the work
   required!
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
    Time.every (100 * millisecond) (always Move)



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
                [ "border" => "1px solid black"
                , "border-collapse" => "collapse"
                , "margin" => "auto"
                ]
            ]
            [ tr []
                (List.map (cell model) [0..model.board - 1])
            ]
        ]


cell : Model -> Int -> Html Msg
cell { snake, apple } index =
    let
        isSnakey =
            List.member index snake.body

        isAppley =
            index == apple.position

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
