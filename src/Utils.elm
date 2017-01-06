module Utils exposing (maybeToCmd, onEnter)

{-| Utility functions for all the codebase.

## Other
@docs maybeToCmd, onEnter
-}

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json
import Keyboard.Extra as Kbd
import Keyboard.Combo as Kbd
import Task


{-| Convert `Just action` to a command performing that action, `Nothing` results in an empty command.
-}
maybeToCmd : Maybe msg -> Cmd.Cmd msg
maybeToCmd action =
    case action of
        Nothing ->
            Cmd.none

        Just action ->
            Task.perform identity (Task.succeed action)


{-| Handle that enter was pressed.
-}
onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if Kbd.fromCode code == Kbd.enter then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)
