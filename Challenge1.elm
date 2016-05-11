module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Html.App as Html
import Json.Decode as Json
import Mouse exposing (Position)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model


type alias Model =
  { isLeft : Bool
  , position : Position
  }


init : (Model, Cmd Msg)
init =
    (Model False (Position 0 0), Cmd.none)


-- Update


type Msg
  = SetMousePosition Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetMousePosition xy ->
      ({ model | position = xy }, Cmd.none)


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- View


view : Model -> Html Msg
view model =
  div
    [ onMouseMove
    , style
      [ "width" => "100%"
      , "height" => "100%"
      , "background-color" => "#000"
      , "display" => "flex"
      , "align-items" => "center"
      , "justify-content" => "center"
      ]
    ]
    [ h1
       [ style
         [ "color" => "#AAA"
         , "font-family" => "Sans-Serif"
         ]
       ]
       [ text (centerText model) ]
     ]


centerText : Model -> String
centerText model =
  toString model.position


onMouseMove =
  on "mousemove" (Json.map SetMousePosition Mouse.position)


-- slightly easier tuples
(=>) : a -> b -> (a, b)
(=>) = (,)
