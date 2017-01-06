module AuthForm exposing (Model, Msg(..), SignInError(..), initModel, changeLang, update, authForm)

{-| This module takes care of user authentication.

## Types
@docs Model, Msg, SignInError

## Model
@docs initModel, changeLang

## Update
@docs update

## View
@docs authForm
-}

import Api
import Html exposing (Html, a, article, button, div, h1, h2, header, img, input, li, nav, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Locale exposing (translate)
import Localization.Language exposing (Language, nativeName)
import Messages
import Types exposing (..)
import Utils exposing (onEnter)


{-| Model of the authentication module.
-}
type alias Model =
    { host : String
    , lang : Language
    , credentials : Credentials
    , error : Maybe SignInError
    }


{-| Actions
-}
type Msg
    = Authenticate
    | UpdateUserName UserName
    | UpdatePassword Password
    | AuthSucceeded Credentials
    | AuthFailed SignInError


{-| Reason why the authentication failed
-}
type SignInError
    = WrongCredentials
    | HttpError Http.Error


{-| Initialise model
-}
initModel : String -> Language -> Model
initModel host lang =
    { host = host
    , lang = lang
    , credentials = { username = "", password = "" }
    , error = Nothing
    }


{-| Change language of the component
-}
changeLang : Language -> Model -> Model
changeLang lang model =
    { model | lang = lang }


{-| Update module state based on an action.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Authenticate ->
            ( model, authenticate model )

        UpdateUserName username ->
            let
                credentials =
                    model.credentials

                newCredentials =
                    { credentials | username = username }
            in
                ( { model | credentials = newCredentials }, Cmd.none )

        UpdatePassword password ->
            let
                credentials =
                    model.credentials

                newCredentials =
                    { credentials | password = password }
            in
                ( { model | credentials = newCredentials }, Cmd.none )

        AuthSucceeded _ ->
            ( { model | error = Nothing }, Cmd.none )

        AuthFailed err ->
            ( { model | error = Just err }, Cmd.none )


{-| The view representing authentication form
-}
authForm : (Msg -> msg) -> Model -> Html msg
authForm msg model =
    let
        error_msg =
            case model.error of
                Nothing ->
                    []

                Just WrongCredentials ->
                    [ Html.p [] [ text <| translate model.lang Messages.IncorrectCredentials ] ]

                Just (HttpError e) ->
                    [ Html.p [] [ text (toString e) ] ]
    in
        Html.form [ class "auth-form" ] <|
            error_msg
                ++ [ Html.div [ class "form-group" ]
                        [ Html.label [ for "auth-form-username" ] [ text <| translate model.lang Messages.UserName ]
                        , Html.div [ class "form-control" ] [ input [ id "auth-form-username", type_ "text", onInput (\username -> msg (UpdateUserName username)), value model.credentials.username, onEnter (msg Authenticate) ] [] ]
                        ]
                   , Html.div [ class "form-group" ]
                        [ Html.label [ for "auth-form-password" ] [ text <| translate model.lang Messages.Password ]
                        , Html.div [ class "form-control" ] [ input [ id "auth-form-password", type_ "password", onInput (\password -> msg (UpdatePassword password)), value model.credentials.password, onEnter (msg Authenticate) ] [] ]
                        ]
                   , Html.div [ class "form-group" ]
                        [ button [ type_ "button", onClick (msg Authenticate) ] [ text <| translate model.lang Messages.SignIn ]
                        ]
                   ]


{-| Verify the credentials on the server
-}
authenticate : Model -> Cmd Msg
authenticate { credentials, host } =
    let
        requestModel =
            { credentials = Nothing, host = host }
    in
        Api.login
            requestModel
            credentials
            (\res ->
                case res of
                    Ok True ->
                        AuthSucceeded credentials

                    Ok False ->
                        AuthFailed WrongCredentials

                    Err e ->
                        AuthFailed (HttpError e)
            )
