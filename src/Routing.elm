module Routing exposing (Route(..), ItemListState, parseLocation, link, routesEqual)

{-| This module defines the type representing route and provides functions for converting the location to route and vice versa.

@docs Route, ItemListState, parseLocation, link, routesEqual
-}

import Navigation exposing (Location)
import Http
import RemoteData
import SourceList
import Types exposing (..)
import UrlParser exposing (..)


{-| -}
type alias ItemListState =
    { activeItem : Maybe Int
    , filter : Filter
    , items : RemoteData.WebData (List DisplayItem)
    }


{-| -}
type Route
    = ItemList ItemListState
    | SourceList SourceList.Model
    | AuthError
    | NotFoundRoute


{-| Convert URL fragment to route.
-}
parseLocation : Location -> Route
parseLocation location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map (ItemList { activeItem = Nothing, filter = defaultFilter, items = RemoteData.NotAsked }) UrlParser.top
        , UrlParser.map AuthError (UrlParser.s "auth")
        , UrlParser.map (SourceList (SourceList.init "" Nothing)) (UrlParser.s "sources")
        , UrlParser.map (\primary tagName -> ItemList { activeItem = Nothing, filter = { primary = primary, secondary = OnlyTag (Maybe.withDefault "" <| Http.decodeUri tagName) }, items = RemoteData.NotAsked }) (primaryFilter </> UrlParser.s "tag" </> UrlParser.string)
        , UrlParser.map (\primary sourceId -> ItemList { activeItem = Nothing, filter = { primary = primary, secondary = OnlySource sourceId }, items = RemoteData.NotAsked }) (primaryFilter </> UrlParser.s "source" </> UrlParser.int)
        , UrlParser.map (\primary -> ItemList { activeItem = Nothing, filter = { primary = primary, secondary = AllTags }, items = RemoteData.NotAsked }) (primaryFilter </> UrlParser.s "all")
        ]


primaryFilter : UrlParser.Parser (FilterPrimary -> a) a
primaryFilter =
    UrlParser.custom "PRIMARY_FILTER"
        (\segment ->
            case segment of
                "all" ->
                    Ok AllItems

                "unread" ->
                    Ok UnreadItems

                "starred" ->
                    Ok StarredItems

                _ ->
                    Err "Invalid primary filter"
        )


{-| Convert route to URL fragment.
-}
link : Route -> String
link route =
    case route of
        ItemList { activeItem, filter } ->
            let
                primary =
                    case filter.primary of
                        AllItems ->
                            "all"

                        UnreadItems ->
                            "unread"

                        StarredItems ->
                            "starred"

                secondary =
                    case filter.secondary of
                        AllTags ->
                            "all"

                        OnlySource sourceId ->
                            "source/" ++ toString sourceId

                        OnlyTag tagName ->
                            "tag/" ++ Http.encodeUri tagName
            in
                "#" ++ primary ++ "/" ++ secondary

        SourceList _ ->
            "#sources"

        AuthError ->
            "#auth"

        NotFoundRoute ->
            "#404"


{-| Test routes for equality.

Ignores internal state stored in the route.
-}
routesEqual : Route -> Route -> Bool
routesEqual a b =
    let
        stripState route =
            case route of
                ItemList listData ->
                    ItemList { listData | activeItem = Nothing, items = RemoteData.NotAsked }

                _ ->
                    route
    in
        stripState a == stripState b
