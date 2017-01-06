module Scroll exposing (intoView)

{-| This module provides a method for scrolling an element into view.

It is similar to `Dom.Scroll` module.

@docs intoView
-}

import Dom exposing (Error, Id)
import Task exposing (Task)
import Native.Scroll


{-| Scroll a node into the view.

This is roughly the same as saying [`document.getElementById(id).scrollIntoView`][docs].

[docs]: https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView
-}
intoView : Id -> Task Error ()
intoView =
    Native.Scroll.intoView
