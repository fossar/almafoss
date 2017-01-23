module Types
    exposing
        ( UserName
        , Password
        , Credentials
        , FilterPrimary(..)
        , FilterSecondary(..)
        , Filter
        , defaultFilter
        , Item
        , DisplayItem
        , Tag
        , Source
        , SourceData
        , Spout
        , SpoutParam
        , Stats
        , Mapper
        )

{-| Common types and aliases.

## Authentication
@docs UserName, Password, Credentials

## Filters
@docs FilterPrimary, FilterSecondary, Filter, defaultFilter

## Feed data
@docs Item, DisplayItem, Tag, Source, SourceData, Spout, SpoutParam, Stats

## Other
@docs Mapper
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


{-| -}
type FilterPrimary
    = AllItems
    | UnreadItems
    | StarredItems


{-| -}
type FilterSecondary
    = AllTags
    | OnlySource Int
    | OnlyTag String


{-| -}
type alias Filter =
    { primary : FilterPrimary
    , secondary : FilterSecondary
    }


{-| -}
defaultFilter : Filter
defaultFilter =
    { primary = AllItems
    , secondary = AllTags
    }


{-| -}
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


{-| -}
type alias DisplayItem =
    { item : Item
    , open : Bool
    }


{-| -}
type alias Tag =
    { tag : String
    , color : String
    , unread : Int
    }


{-| -}
type alias Source =
    { id : Int
    , title : String
    , unread : Int
    }


{-| -}
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


{-| -}
type alias Spout =
    { name : String
    , description : String
    , params : Dict String SpoutParam
    }


{-| -}
type alias SpoutParam =
    { title : String
    , class : String
    , default : String
    , required : Bool
    , validation : List String
    }


{-| -}
type alias Stats =
    { total : Int
    , unread : Int
    , starred : Int
    }


{-| -}
type alias Mapper a b =
    (b -> b) -> a -> a
