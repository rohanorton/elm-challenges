module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App as Html
import Time exposing (Time, second)
import Random


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
  List Circle


type alias Circle =
  { position : Position
  , size : Int
  , color : String
  }


type alias Position =
  { x : Int
  , y : Int
  }

init : (Model, Cmd Msg)
init =
    ([], Cmd.none)


-- Update

type Msg
  = AddCircle (List Int)
  | GenerateRandom Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenerateRandom float ->
      (model, Random.generate AddCircle (Random.list 4 (Random.int 1 100)))

    AddCircle (rand1 :: rand2 :: rand3 :: rand4 :: []) ->
      let
          newCircle = { position = (Position (rand1 * 7) (rand2 * 3)), size =
            (rand3), color =
            "cornflowerblue" }
      in
          (newCircle :: model , Cmd.none)

    AddCircle _ ->
      (model, Cmd.none)


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second GenerateRandom


-- View


view : Model -> Html Msg
view model =
  let mycircle = circle { position = (Position 12 39), color = "blue", size = 25 }
  in
  body
    []
    (List.map circle model)


circle : Circle -> Html msg
circle { position, size, color } =
  div
    [ style
      [ "border-radius" => "50%"
      , "width" => (px size)
      , "height" => (px size)
      , "background-color" => color
      , "border" => "1px solid white"
      , "position" => "absolute"
      , "left" => (px position.x)
      , "top" => (px position.y)
      ]
    ]
    []


-- slightly easier tuples
(=>) : a -> b -> (a, b)
(=>) = (,)

px : a -> String
px x =
  toString x ++ "px"

