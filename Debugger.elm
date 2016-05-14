module Debugger exposing (debugger)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

debugger : model -> Bool -> Html msg
debugger model debug =
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
        , "max-width" => "30%"
        , "max-height" => "30%"
        , "overflow" => "auto"
        , "z-index" => "100000"
        ]
      ]
      [ text (toString model) ]
  else
    text ""


-- slightly easier tuples
(=>) : a -> b -> (a, b)
(=>) = (,)
