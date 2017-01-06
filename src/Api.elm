module Api exposing (items, sources, sourceData, tags, stats, markItemRead, markItemUnread, starItem, unstarItem, login)

{-| Module handling communication with Selfoss using [REST API][rest-docs].

@docs items, sources, sourceData, tags, stats, markItemRead, markItemUnread, starItem, unstarItem, login

[rest-docs]: https://github.com/SSilence/selfoss/wiki/Restful-API-for-Apps-or-any-other-external-access
-}

import Http
import HttpBuilder exposing (..)
import Json.Decode as Json
import Json.Decode as Json exposing (field)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, required, optional)
import Time.DateTime exposing (DateTime, fromTimestamp)
import Types exposing (..)
import Utils


items : { model | credentials : Maybe Credentials, host : String } -> Filter -> (Result Http.Error (List Item) -> msg) -> Cmd msg
items { credentials, host } filter handler =
    HttpBuilder.get (host ++ "/items")
        |> withQueryParams (makeAuth credentials ++ filterToParams filter)
        |> withExpect (Http.expectJson itemsDecoder)
        |> send handler


sources : { model | credentials : Maybe Credentials, host : String } -> (Result Http.Error (List Source) -> msg) -> Cmd msg
sources { credentials, host } handler =
    HttpBuilder.get (host ++ "/sources/stats")
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson sourcesDecoder)
        |> send handler


sourceData : { model | credentials : Maybe Credentials, host : String } -> (Result Http.Error (List SourceData) -> msg) -> Cmd msg
sourceData { credentials, host } handler =
    HttpBuilder.get (host ++ "/sources/list")
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson sourceDataDecoder)
        |> send handler


tags : { model | credentials : Maybe Credentials, host : String } -> (Result Http.Error (List Tag) -> msg) -> Cmd msg
tags { credentials, host } handler =
    HttpBuilder.get (host ++ "/tags")
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson tagsDecoder)
        |> send handler


stats : { model | credentials : Maybe Credentials, host : String } -> (Result Http.Error Stats -> msg) -> Cmd msg
stats { credentials, host } handler =
    HttpBuilder.get (host ++ "/stats")
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson statsDecoder)
        |> send handler


markItemRead : { model | credentials : Maybe Credentials, host : String } -> Item -> (Result Http.Error Bool -> msg) -> Cmd msg
markItemRead { credentials, host } item handler =
    HttpBuilder.post (host ++ "/mark/" ++ toString item.id)
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson successDecoder)
        |> send handler


markItemUnread : { model | credentials : Maybe Credentials, host : String } -> Item -> (Result Http.Error Bool -> msg) -> Cmd msg
markItemUnread { credentials, host } item handler =
    HttpBuilder.post (host ++ "/unmark/" ++ toString item.id)
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson successDecoder)
        |> send handler


starItem : { model | credentials : Maybe Credentials, host : String } -> Item -> (Result Http.Error Bool -> msg) -> Cmd msg
starItem { credentials, host } item handler =
    HttpBuilder.post (host ++ "/starr/" ++ toString item.id)
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson successDecoder)
        |> send handler


unstarItem : { model | credentials : Maybe Credentials, host : String } -> Item -> (Result Http.Error Bool -> msg) -> Cmd msg
unstarItem { credentials, host } item handler =
    HttpBuilder.post (host ++ "/unstarr/" ++ toString item.id)
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson successDecoder)
        |> send handler


login : { model | host : String } -> Credentials -> (Result Http.Error Bool -> msg) -> Cmd msg
login { host } credentials handler =
    HttpBuilder.get (host ++ "/login")
        |> withQueryParams [ ( "username", credentials.username ), ( "password", credentials.password ) ]
        |> withExpect (Http.expectJson successDecoder)
        |> send handler



-- HELPERS


makeAuth : Maybe Credentials -> List ( String, String )
makeAuth credentials =
    case credentials of
        Just { username, password } ->
            [ ( "username", username ), ( "password", password ) ]

        Nothing ->
            []


filterToParams : Filter -> List ( String, String )
filterToParams { primary, secondary } =
    let
        showPrimary pri =
            case pri of
                AllItems ->
                    [ ( "type", "newest" ) ]

                UnreadItems ->
                    [ ( "type", "unread" ) ]

                StarredItems ->
                    [ ( "type", "starred" ) ]

        showSecondary sec =
            case sec of
                AllTags ->
                    []

                OnlySource sourceId ->
                    [ ( "source", toString sourceId ) ]

                OnlyTag tagName ->
                    [ ( "tag", tagName ) ]
    in
        showPrimary primary ++ showSecondary secondary



-- DECODERS


sourcesDecoder : Json.Decoder (List Source)
sourcesDecoder =
    let
        sourceDecoder =
            Json.map3 Source
                (field "id" intString)
                (field "title" Json.string)
                (field "unread" intString)
    in
        Json.list sourceDecoder


sourceDataDecoder : Json.Decoder (List SourceData)
sourceDataDecoder =
    let
        sourceDecoder =
            decode SourceData
                |> required "id" intString
                |> required "title" Json.string
                |> required "tags" tagListDecoder
                |> required "spout" Json.string
                |> required "params" (Json.dict (Json.maybe Json.string))
                |> required "error" maybeString
                |> required "lastentry" (Json.maybe datetime)
                |> required "icon" (Json.maybe Json.string)
    in
        Json.list sourceDecoder


tagsDecoder : Json.Decoder (List Tag)
tagsDecoder =
    let
        tagDecoder =
            Json.map3 Tag
                (field "tag" Json.string)
                (field "color" Json.string)
                (field "unread" intString)
    in
        Json.list tagDecoder


statsDecoder : Json.Decoder Stats
statsDecoder =
    Json.map3 Stats
        (field "total" intString)
        (field "unread" intString)
        (field "starred" intString)


itemsDecoder : Json.Decoder (List Item)
itemsDecoder =
    let
        itemDecoder =
            decode Item
                |> required "id" intString
                |> required "title" Json.string
                |> required "link" Json.string
                |> required "content" Json.string
                |> required "author" (Json.map Just Json.string)
                |> required "datetime" Json.string
                |> required "tags" tagListDecoder
                |> required "unread" fakeBool
                |> required "starred" fakeBool
                |> custom
                    (decode Source
                        |> required "source" intString
                        |> required "sourcetitle" Json.string
                        |> hardcoded 0
                    )
                |> required "icon" Json.string
    in
        Json.list itemDecoder


tagListDecoder : Json.Decoder (List String)
tagListDecoder =
    Json.map
        (\tags ->
            if String.isEmpty tags then
                []
            else
                String.split "," tags
        )
        Json.string


{-| Parse the basic response.

For certain requests, Selfoss responds with either `{"success": true}` or `{"success": false}`.
-}
successDecoder : Json.Decoder Bool
successDecoder =
    Json.field "success" Json.bool


{-| Ignore empty string.
-}
maybeString : Json.Decoder (Maybe String)
maybeString =
    Json.map
        (\str ->
            if String.isEmpty str then
                Nothing
            else
                Just str
        )
        Json.string


{-| Decode `"1"` and `"0"` into corresponding boolean value.
-}
fakeBool : Json.Decoder Bool
fakeBool =
    Json.string
        |> Json.andThen
            (\b ->
                case b of
                    "1" ->
                        Json.succeed True

                    "0" ->
                        Json.succeed False

                    _ ->
                        Json.fail ("Expecting either \"0\" or \"1\", " ++ b ++ " given.")
            )


{-| Decode number written as string.
-}
intString : Json.Decoder Int
intString =
    Json.string
        |> Json.andThen (\i -> resultToJson <| String.toInt i)


{-| Decode float written as string.
-}
floatString : Json.Decoder Float
floatString =
    Json.string
        |> Json.andThen (\i -> resultToJson <| String.toFloat i)


{-| Convert result into a JSON decoder.
-}
resultToJson : Result String a -> Json.Decoder a
resultToJson r =
    case r of
        Ok o ->
            Json.succeed o

        Err e ->
            Json.fail e


{-| Decode string containing UNIX timestamp to a `DateTime`.
-}
datetime : Json.Decoder DateTime
datetime =
    Json.map fromTimestamp floatString
