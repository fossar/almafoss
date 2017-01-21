module Routing exposing (Route(..), parseLocation, link)

{-| This module defines the type representing route and provides functions for converting the location to route and vice versa.

@docs Route, parseLocation, link
-}

import Navigation exposing (Location)
import Http
import Types exposing (..)
import UrlParser exposing (..)


{-| -}
type Route
    = ItemList { activeItem : Maybe Int, filter : Filter }
    | SourceList
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
        [ UrlParser.map (ItemList { activeItem = Nothing, filter = defaultFilter }) UrlParser.top
        , UrlParser.map AuthError (UrlParser.s "auth")
        , UrlParser.map SourceList (UrlParser.s "sources")
        , UrlParser.map (\primary tagName -> ItemList { activeItem = Nothing, filter = { primary = primary, secondary = OnlyTag (Maybe.withDefault "" <| Http.decodeUri tagName) } }) (primaryFilter </> UrlParser.s "tag" </> UrlParser.string)
        , UrlParser.map (\primary sourceId -> ItemList { activeItem = Nothing, filter = { primary = primary, secondary = OnlySource sourceId } }) (primaryFilter </> UrlParser.s "source" </> UrlParser.int)
        , UrlParser.map (\primary -> ItemList { activeItem = Nothing, filter = { primary = primary, secondary = AllTags } }) (primaryFilter </> UrlParser.s "all")
        , UrlParser.map (\primary tagName activeItem -> ItemList { activeItem = Just activeItem, filter = { primary = primary, secondary = OnlyTag (Maybe.withDefault "" <| Http.decodeUri tagName) } }) (primaryFilter </> UrlParser.s "tag" </> UrlParser.string </> UrlParser.int)
        , UrlParser.map (\primary sourceId activeItem -> ItemList { activeItem = Just activeItem, filter = { primary = primary, secondary = OnlySource sourceId } }) (primaryFilter </> UrlParser.s "source" </> UrlParser.int </> UrlParser.int)
        , UrlParser.map (\primary activeItem -> ItemList { activeItem = Just activeItem, filter = { primary = primary, secondary = AllTags } }) (primaryFilter </> UrlParser.s "all" </> UrlParser.int)
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

                active =
                    case activeItem of
                        Just activeId ->
                            "/" ++ toString activeId

                        Nothing ->
                            ""
            in
                "#" ++ primary ++ "/" ++ secondary ++ active

        SourceList ->
            "#sources"

        AuthError ->
            "#auth"

        NotFoundRoute ->
            "#404"
