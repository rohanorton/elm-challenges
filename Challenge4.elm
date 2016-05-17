module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style, src)
import Html.App as Html
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Debugger exposing (debugger)
import Time exposing (Time)
import Task
import Process


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
    , user : Maybe User
    , debounce : Debounce
    }


type alias Debounce =
    { invocations : Int
    , duration : Time
    }


type alias User =
    { name : String
    , avatarUrl : String
    }


init : ( Model, Cmd Msg )
init =
    { username = ""
    , user = Nothing
    , debounce = Debounce 0 (1 * Time.second)
    }
        ! []



-- Update


type Msg
    = NoOp
    | SetUsername String
    | Submit
    | FetchSucceed ( String, String )
    | FetchFail Http.Error
    | DebounceInvoke
    | DebounceComplete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetUsername username ->
            { model | username = username } ! [ wrap DebounceInvoke ]

        Submit ->
            model ! [ getGithubUser model.username ]

        FetchFail err ->
            { model | user = Nothing } ! []

        FetchSucceed ( name, avatarUrl ) ->
            { model
                | username = ""
                , user = Just (User name avatarUrl)
            }
                ! []

        DebounceInvoke ->
            let
                { invocations, duration } =
                    model.debounce
            in
                { model | debounce = Debounce (invocations + 1) duration }
                    ! [ delay duration DebounceComplete ]

        DebounceComplete ->
            let
                { invocations, duration } =
                    model.debounce

                cmds =
                    if invocations == 1 then
                        [ wrap Submit ]
                    else
                        []
            in
                { model | debounce = Debounce (invocations - 1) duration } ! cmds


wrap : Msg -> Cmd Msg
wrap msg =
    Task.succeed ()
        |> Task.perform (always msg) (always msg)


delay : Time -> Msg -> Cmd Msg
delay duration msg =
    Process.sleep duration
        |> Task.perform (always msg) (always msg)


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
        ]


renderUser : Maybe User -> Html msg
renderUser maybe =
    case maybe of
        Nothing ->
            text ""

        Just { name, avatarUrl } ->
            section []
                [ h2 [] [ text name ]
                , img [ src avatarUrl ] []
                ]


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
