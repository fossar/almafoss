module Model exposing (..)

import Items.Model exposing (Item)
import Routing
import Localization
import AuthForm


type alias Model =
    { host : String
    , lang : Localization.Lang
    , page : Page
    , items : List Item
    , tags : List String
    , sources : List Source
    , stats : Stats
    , credentials : Maybe AuthForm.Credentials
    , authForm : AuthForm.Model
    , combos : Kbd.Model Msg
    }


initialModel : Routing.Route -> Model
initialModel route =
    { players = []
    , route = route
    }
