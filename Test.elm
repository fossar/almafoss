module Main exposing (main)

import Html exposing (Html, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


type alias Model =
    { username : String
    }


init : Model
init =
    { username = "" }


type Msg
    = UpdateUserName String


update : Msg -> Model -> Model
update action model =
    case action of
        UpdateUserName username ->
            { model | username = username }


view : Model -> Html Msg
view model =
    let
        error_msg =
            if String.length model.username > 5 then
                [ Html.p [] [ text "Username too long." ] ]
            else
                []
    in
        Html.form [] <|
            error_msg
                ++ [ input [ type_ "text", onInput UpdateUserName, value model.username ] []
                   ]
