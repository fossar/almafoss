module AuthForm exposing (Model, Msg(..), SignInError(..), init, update, authForm, authenticate)

{-| This module takes care of user authentication.

## Types
@docs Model, Msg, SignInError

## Model
@docs init

## Update
@docs update, authenticate

## View
@docs authForm
-}

import Api
import Forms
import Html exposing (Html, a, article, button, div, h1, h2, header, img, input, li, nav, span, text, ul)
import Html.Attributes exposing (..)
import Http
import Locale exposing (translate)
import Localization.Language exposing (Language, nativeName)
import Messages
import Types exposing (..)


{-| Model of the authentication module.
-}
type alias Model =
    { credentials : Credentials
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
init : Model
init =
    { credentials = { username = "", password = "" }
    , error = Nothing
    }


{-| Update module state based on an action.
-}
update : Msg -> Model -> Model
update action model =
    case action of
        Authenticate ->
            Debug.crash "Handled upstream"

        UpdateUserName username ->
            let
                credentials =
                    model.credentials

                newCredentials =
                    { credentials | username = username }
            in
                { model | credentials = newCredentials }

        UpdatePassword password ->
            let
                credentials =
                    model.credentials

                newCredentials =
                    { credentials | password = password }
            in
                { model | credentials = newCredentials }

        AuthSucceeded _ ->
            { model | error = Nothing }

        AuthFailed err ->
            { model | error = Just err }


{-| The view representing authentication form
-}
authForm : (Msg -> msg) -> Model -> Language -> Html msg
authForm msg model lang =
    let
        error_msg =
            case model.error of
                Nothing ->
                    []

                Just WrongCredentials ->
                    [ Html.p [ class "form-message" ] [ text <| translate lang Messages.IncorrectCredentials ] ]

                Just (HttpError e) ->
                    [ Html.p [ class "form-message" ] [ text (toString e) ] ]
    in
        Forms.form (msg Authenticate) <|
            error_msg
                ++ [ Forms.lineEdit
                        { identifier = "auth-form-username"
                        , label = translate lang Messages.UserName
                        , action = (\username -> msg (UpdateUserName username))
                        , value = model.credentials.username
                        }
                   , Forms.password
                        { identifier = "auth-form-password"
                        , label = translate lang Messages.Password
                        , action = (\password -> msg (UpdatePassword password))
                        , value = model.credentials.password
                        }
                   , Forms.button
                        { action = msg Authenticate
                        , label = translate lang Messages.SignIn
                        }
                   ]


{-| Verify the credentials on the server
-}
authenticate : Credentials -> String -> Cmd Msg
authenticate credentials host =
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
