module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style, src)
import Html.App as Html
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Debugger exposing (debugger)


debug : Bool
debug =
    False


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
    { username : String
    , user : User
    }


type alias User =
    { name : String
    , avatarUrl : String
    }


init : ( Model, Cmd Msg )
init =
    { username = ""
    , user = User "" ""
    }
        ! []



-- Update


type Msg
    = NoOp
    | SetUsername String
    | Submit
    | FetchSucceed ( String, String )
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetUsername username ->
            { model | username = username } ! []

        Submit ->
            model ! [ getGithubUser model.username ]

        FetchFail err ->
            { model | user = User "" "" } ! []

        FetchSucceed ( name, avatarUrl ) ->
            { model
                | username = ""
                , user = User name avatarUrl
            }
                ! []


getGithubUser : String -> Cmd Msg
getGithubUser username =
    let
        resource =
            "http://api.github.com/users/" ++ (Http.uriEncode username)
    in
        Http.get decodeResponse resource
            |> Task.perform FetchFail FetchSucceed


decodeResponse : Json.Decoder ( String, String )
decodeResponse =
    Json.object2 (,)
        ("name" := Json.string)
        ("avatar_url" := Json.string)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view ({ user } as model) =
    body
        [ style
            [ "font-family" => "Sans-Serif"
            , "padding" => "20px"
            ]
        ]
        [ renderForm
        , renderUser user
        , debugger model debug
        ]


renderForm : Html Msg
renderForm =
    div []
        [ label [] [ text "Github Username: " ]
        , input [ onInput SetUsername ] []
        , button [ onClick Submit ] [ text "Submit" ]
        ]


renderUser : User -> Html msg
renderUser { name, avatarUrl } =
    div []
        [ p [] [ text name ]
        , img [ src avatarUrl ] []
        ]


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
