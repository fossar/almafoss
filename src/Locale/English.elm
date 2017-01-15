module Locale.English exposing (translator)

{-| English translation of the application.

@docs translator
-}

import Localization exposing (..)
import Localization.Pluralization as Pluralization
import Messages exposing (..)


pl : Int -> String -> String -> Translation
pl n one more =
    t (Pluralization.english n one more)


{-| -}
translator : Translator Message
translator str =
    case str of
        AppName ->
            t "álmafoss"

        Title page ->
            case page of
                Nothing ->
                    t "álmafoss"

                Just title ->
                    t (title ++ " – álmafoss")

        NotFoundTitle ->
            t "Page not found"

        NotFound ->
            t "Requested page could not be found."

        AuthenticationTitle ->
            t "Sign in"

        UserName ->
            t "User name"

        Password ->
            t "Password"

        SignIn ->
            t "Sign in"

        SignOut ->
            t "Sign out"

        AuthenticationRequired ->
            t "You do not have permissions to carry out this action. Please sign in."

        IncorrectCredentials ->
            t "Incorrect credentials entered."

        Filters ->
            t "Filters"

        All ->
            t "All"

        Unread ->
            t "Unread"

        Starred ->
            t "Starred"

        Tags ->
            t "Tags"

        AllTags ->
            t "All tags"

        Sources ->
            t "Sources"

        NumberUnread n ->
            t (toString n ++ " unread")

        Refresh ->
            t "Refresh"

        RefreshTags ->
            t "Refresh tags"

        RefreshSources ->
            t "Refresh sources"

        ItemListTitle ->
            t "List of items"

        Loading ->
            t "Loading content…"

        NoEntries ->
            t "No entries found."

        UnstarEntry ->
            t "Unstar"

        StarEntry ->
            t "Star"

        MarkEntryRead ->
            t "Mark as read"

        MarkEntryUnread ->
            t "Mark as unread"

        ManageSources ->
            t "Manage sources"

        SourceListTitle ->
            t "List of sources"

        AddSource ->
            t "Add source"

        NoSources ->
            t "No sources found."

        EditSource ->
            t "Edit"

        SaveSource ->
            t "Save"

        SourceSaved ->
            t "Changes saved"

        CancelSourceEditing ->
            t "Cancel"

        DeleteSource ->
            t "Delete"

        ReallyDeleteSource ->
            t "Do you really wish to delete the source?"

        SourceTitle ->
            t "Title"

        SourceTags ->
            t "Tags"

        SourceFilter ->
            t "Filter"

        SourceSpout ->
            t "Spout"
