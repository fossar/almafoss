module Types exposing (..)

{-| Common types and aliases.
-}

import Dict exposing (Dict)
import Time.DateTime exposing (DateTime)

{-| User name
-}
type alias UserName =
    String


{-| Password
-}
type alias Password =
    String


{-| Credentials consist of a user name and a password.
-}
type alias Credentials =
    { username : UserName
    , password : Password
    }


type FilterPrimary
    = AllItems
    | UnreadItems
    | StarredItems


type FilterSecondary
    = AllTags
    | OnlySource Int
    | OnlyTag String


type alias Filter =
    { primary : FilterPrimary
    , secondary : FilterSecondary
    }


type alias Item =
    { id : Int
    , title : String
    , url : String
    , content : String
    , author : Maybe String
    , datetime : String
    , tags : List String
    , unread : Bool
    , starred : Bool
    , source : Source
    , icon : String
    }


type alias Tag =
    { tag : String
    , color : String
    , unread : Int
    }


type alias Source =
    { id : Int
    , title : String
    , unread : Int
    }


type alias SourceData =
    { id : Int
    , title : String
    , tags : List String
    , spout : String
    , params : Dict String (Maybe String)
    , error : Maybe String
    , lastentry : Maybe DateTime
    , icon : Maybe String
    }


type alias Stats =
    { total : Int
    , unread : Int
    , starred : Int
    }


defaultFilter : Filter
defaultFilter =
    { primary = AllItems
    , secondary = AllTags
    }
