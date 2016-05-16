module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App as Html
import Time exposing (Time, millisecond)
import Random
import Window exposing (Size)
import Task
import Debugger exposing (debugger)
import Keyboard exposing (KeyCode)
import Char

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
  , paused : Bool
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
    { circles = [], windowSize = Size 0 0, paused = False } !
    [ Task.perform (always NoOp) SetWindowSize Window.size ]

-- Update

type Msg
  = AddCircle (Int, Int, Int, Int)
  | GenerateRandom Float
  | SetWindowSize Size
  | OnKey Char
  | Pause
  | Reset
  | NoOp


randomInt : Random.Generator Int
randomInt = Random.int 1 100


quadTuple : a -> b -> c -> d -> (a, b, c, d)
quadTuple w x y z =
  (w, x, y, z)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenerateRandom float ->
      ( model
      , Random.generate AddCircle (Random.map4 quadTuple randomInt randomInt randomInt randomInt) )

    AddCircle (w, x, y, z) ->
      if model.paused then
        update NoOp model
      else
        let
            newCircle = createCircle model.windowSize w x y z
        in
            ({ model | circles = newCircle :: model.circles } , Cmd.none)

    SetWindowSize size ->
      ( { model | windowSize = size }
      , Cmd.none )

    Pause ->
      ( { model | paused = not model.paused } , Cmd.none )

    Reset ->
      ( { model | circles = [] } , Cmd.none )

    OnKey key ->
      case key of
        'p' ->
          update Pause model
        'r' ->
          update Reset model
        _ ->
          update NoOp model

    NoOp ->
      model ! []


createCircle : Size -> Int -> Int -> Int -> Int -> Circle
createCircle { width, height } int1 int2 int3 int4 =
  { position =
    { x = int1 * width // 100 - 100
    , y = int2 * height // 100 - 100
    }
      , size = int3 * 2
      , color = "rebeccapurple"
  }


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (200 * millisecond) GenerateRandom
    , Keyboard.presses (OnKey << Char.fromCode)
    ]


-- View


view : Model -> Html Msg
view ({ circles } as model) =
  body
    []
    ( buttons model ::
      debugger model debug ::
      (List.map circle circles) )

buttons : Model -> Html Msg
buttons { paused } =
  div
    [ style
      [ "z-index" => "100000"
      , "position" => "relative"
      ]
    ]
    [ button [ onClick Reset ] [ text "Reset" ]
    , button [ onClick Pause ] [ text (pauseText paused) ]
    ]


pauseText : Bool -> String
pauseText paused =
  if paused then
    "Unpause"
  else
    "Pause"


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

