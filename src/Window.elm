module Window exposing (open)

{-| This module provides a method for opening a windows.

@docs open
-}

import Task exposing (Task)
import Native.Window


type Error
    = CannotOpen


{-| Open a new window.

This is roughly the same as saying [`window.open`][docs].

[docs]: https://developer.mozilla.org/en-US/docs/Web/API/Window/open
-}
open : String -> Task Error ()
open =
    Native.Window.open
