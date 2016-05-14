module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App as Html
import Time exposing (Time, second)
import Random
import Window exposing (Size)
import Task
import Debugger exposing (debugger)


debug : Bool
debug = False


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
  { circles : List Circle
  , windowSize : Size
  }


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
    ({ circles = [], windowSize = Size 0 0 }
    , Task.perform Fail SetWindowSize Window.size
    )

-- Update

type Msg
  = AddCircle (List Int)
  | GenerateRandom Float
  | SetWindowSize Size
  | Fail ()


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenerateRandom float ->
      (model, Random.generate AddCircle (Random.list 4 (Random.int 1 100)))

    AddCircle (rand1 :: rand2 :: rand3 :: rand4 :: []) ->
      let
          newCircle =
            { position =
              { x = rand1 * model.windowSize.width // 100 - 100
              , y = rand2 * model.windowSize.height // 100 - 100
              }
            , size = rand3 * 2
            , color = "cornflowerblue"
            }
      in
          ({ model | circles = newCircle :: model.circles } , Cmd.none)

    SetWindowSize size ->
      ( { model | windowSize = size }
      , Cmd.none )

    _ ->
      (model, Cmd.none)


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second GenerateRandom


-- View


view : Model -> Html Msg
view ({ circles } as model) =
  body
    []
    ((debugger model debug) :: (List.map circle circles))


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

