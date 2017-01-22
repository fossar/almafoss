module Messages exposing (Message(..))

{-| This module contains all the messages used by the application

@docs Message
-}


{-| Union of all the messages used by the application.
-}
type Message
    = AppName
    | NotFoundTitle
    | Title (Maybe String)
    | NotFound
    | AuthenticationTitle
    | UserName
    | Password
    | SignIn
    | SignOut
    | AuthenticationRequired
    | IncorrectCredentials
    | Filters
    | Unread
    | All
    | Starred
    | Tags
    | AllTags
    | Sources
    | NumberUnread Int
    | Refresh
    | ItemListTitle
    | Loading
    | NoEntries
    | UnstarEntry
    | StarEntry
    | MarkEntryRead
    | MarkEntryUnread
    | ManageSources
    | SourceListTitle
    | AddSource
    | NoSources
    | EditSource
    | SaveSource
    | SourceSaved
    | CancelSourceEditing
    | DeleteSource
    | ReallyDeleteSource
    | SourceTitle
    | SourceTags
    | SourceFilter
    | SourceSpout
