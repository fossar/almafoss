module Main exposing (..)

import Html exposing (Html, img)
import Html.Accessibility exposing (..)
import Html.Attributes exposing (..)


sign_in : Html msg
sign_in =
    img [ src "assets/icons/sign-in.svg", alt "", ariaHidden True ] []
