module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Html.App as Html
import Json.Decode as Json
import Mouse exposing (Position)
import Window exposing (Size)
import Task


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
  { isLeft : Bool
  , position : Position
  , windowSize : Size
  }


init : (Model, Cmd Msg)
init =
    ( Model True (Position 0 0) (Size 0 0)
    , Task.perform Fail SetWindowSize Window.size
    )


-- Update


type Msg
  = SetMousePosition Position
  | SetWindowSize Size
  | Fail ()


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetMousePosition xy ->
      ({ model | position = xy
      , isLeft = isLeft model
      }, Cmd.none)

    SetWindowSize size ->
          ({ model | windowSize = size
          , isLeft = isLeft model
          }, Cmd.none)

    Fail () ->
      (model, Cmd.none)


isLeft : Model -> Bool
isLeft { position, windowSize } =
  (windowSize.width // 2) - position.x > 0


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  Window.resizes SetWindowSize


-- View


view : Model -> Html Msg
view model =
  let (centreText, color) = getDetails model
  in
    body
      [ onMouseMove
      , style
        [ "background-color" => color
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
         [ text centreText ]
       , debugger model
       ]


debugger : Model -> Html msg
debugger model =
  if debug then
    p
      [ style
        [ "color" => "#AAA"
        , "font-family" => "Sans-Serif"
        , "position" => "fixed"
        ]
      ]
      [ text (toString model) ]
  else
    div [] []



getDetails : Model -> (String, String)
getDetails { isLeft } =
  if isLeft then
    ("Left", red)
  else
    ("Right", blue)


onMouseMove : Attribute Msg
onMouseMove =
  on "mousemove" (Json.map SetMousePosition Mouse.position)


blue : String
blue = "#6C7EC7"


red : String
red = "#FFA27B"


-- slightly easier tuples
(=>) : a -> b -> (a, b)
(=>) = (,)
