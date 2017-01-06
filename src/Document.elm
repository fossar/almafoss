module Document exposing (title)

{-| This module provides a method for setting document title.

@docs title
-}

import Task exposing (Task)
import Native.Document

{-| Set title of the document.

This is roughly the same as assigning to [`document.title`][docs].

[docs]: https://developer.mozilla.org/en-US/docs/Web/API/Document/title
-}
title : String -> Task Never ()
title =
    Native.Document.title
