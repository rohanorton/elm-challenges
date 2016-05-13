module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App as Html
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
  { mousePosition : Position
  , windowSize : Size
  }


init : (Model, Cmd Msg)
init =
    ( Model (Position 0 0) (Size 0 0)
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
      ( { model | mousePosition = xy }
      , Cmd.none )

    SetWindowSize size ->
      ( { model | windowSize = size }
      , Cmd.none )

    Fail () ->
      ( model, Cmd.none )


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes SetWindowSize
    , Mouse.moves SetMousePosition
    ]


-- View


view : Model -> Html Msg
view model =
  let
      { windowSize, mousePosition } = model
      isLeft = windowSize.width // 2 - mousePosition.x > 0
      (centreText, color) = getDetails isLeft
  in
    body
      [ style
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
    div
      [ style
        [ "background-color" => "#000"
        , "color" => "#CCC"
        , "font-family" => "Sans-Serif"
        , "position" => "fixed"
        , "padding" => "5px"
        , "left" => "0"
        , "top" => "0"
        ]
      ]
      [ text (toString model) ]
  else
    div [] []



getDetails : Bool -> (String, String)
getDetails isLeft =
  if isLeft then
    ("Left", red)
  else
    ("Right", blue)


blue : String
blue = "#6C7EC7"


red : String
red = "#FFA27B"


-- slightly easier tuples
(=>) : a -> b -> (a, b)
(=>) = (,)
