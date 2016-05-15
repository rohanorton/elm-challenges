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

    AddCircle random ->
      case random of
        [ w, x, y, z ] ->
          let
              newCircle = createCircle model.windowSize w x y z
          in
              ({ model | circles = newCircle :: model.circles } , Cmd.none)
        _ ->
          Debug.crash "AddCircle expects array of length 4"

    SetWindowSize size ->
      ( { model | windowSize = size }
      , Cmd.none )

    Fail () ->
      (model, Cmd.none)


createCircle : Size -> Int -> Int -> Int -> Int -> Circle
createCircle { width, height } int1 int2 int3 int4 =
  { position =
    { x = int1 * width // 100 - 100
    , y = int2 * height // 100 - 100
    }
      , size = int3 * 2
      , color = "cornflowerblue"
  }

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

