module Api exposing (items, sources, tags, stats, markItemRead, markItemUnread, starItem, unstarItem, sourceData, addSource, updateSource, deleteSource, login)

{-| Module handling communication with Selfoss using [REST API][rest-docs].

@docs items, sources, tags, stats, markItemRead, markItemUnread, starItem, unstarItem, sourceData, addSource, updateSource, deleteSource, login

[rest-docs]: https://github.com/SSilence/selfoss/wiki/Restful-API-for-Apps-or-any-other-external-access
-}

import Dict
import Http
import HttpBuilder exposing (..)
import Json.Decode as Json
import Json.Decode as Json exposing (field)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, required, optional)
import Json.Encode
import Time.DateTime exposing (DateTime, fromTimestamp)
import Types exposing (..)


{-| Request items.
-}
items : { model | credentials : Maybe Credentials, host : String } -> Filter -> (Result Http.Error (List Item) -> msg) -> Cmd msg
items { credentials, host } filter handler =
    HttpBuilder.get (host ++ "/items")
        |> withQueryParams (makeAuth credentials ++ filterToParams filter)
        |> withExpect (Http.expectJson itemsDecoder)
        |> send handler


{-| Request sources to be used in the sidebar.
-}
sources : { model | credentials : Maybe Credentials, host : String } -> (Result Http.Error (List Source) -> msg) -> Cmd msg
sources { credentials, host } handler =
    HttpBuilder.get (host ++ "/sources/stats")
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson sourcesDecoder)
        |> send handler


{-| Request tags.
-}
tags : { model | credentials : Maybe Credentials, host : String } -> (Result Http.Error (List Tag) -> msg) -> Cmd msg
tags { credentials, host } handler =
    HttpBuilder.get (host ++ "/tags")
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson tagsDecoder)
        |> send handler


{-| Request numbers of all, unread and starred items.
-}
stats : { model | credentials : Maybe Credentials, host : String } -> (Result Http.Error Stats -> msg) -> Cmd msg
stats { credentials, host } handler =
    HttpBuilder.get (host ++ "/stats")
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson statsDecoder)
        |> send handler


{-| Request item to be marked as read.
-}
markItemRead : { model | credentials : Maybe Credentials, host : String } -> Item -> (Result Http.Error Bool -> msg) -> Cmd msg
markItemRead { credentials, host } item handler =
    HttpBuilder.post (host ++ "/mark/" ++ toString item.id)
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson successDecoder)
        |> send handler


{-| Request item to be marked as unread.
-}
markItemUnread : { model | credentials : Maybe Credentials, host : String } -> Item -> (Result Http.Error Bool -> msg) -> Cmd msg
markItemUnread { credentials, host } item handler =
    HttpBuilder.post (host ++ "/unmark/" ++ toString item.id)
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson successDecoder)
        |> send handler


{-| Request item to be starred.
-}
starItem : { model | credentials : Maybe Credentials, host : String } -> Item -> (Result Http.Error Bool -> msg) -> Cmd msg
starItem { credentials, host } item handler =
    HttpBuilder.post (host ++ "/starr/" ++ toString item.id)
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson successDecoder)
        |> send handler


{-| Request item to be unstarred.
-}
unstarItem : { model | credentials : Maybe Credentials, host : String } -> Item -> (Result Http.Error Bool -> msg) -> Cmd msg
unstarItem { credentials, host } item handler =
    HttpBuilder.post (host ++ "/unstarr/" ++ toString item.id)
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson successDecoder)
        |> send handler


{-| Request sources to be used on the source management page.
-}
sourceData : { model | credentials : Maybe Credentials, host : String } -> (Result Http.Error (List SourceData) -> msg) -> Cmd msg
sourceData { credentials, host } handler =
    HttpBuilder.get (host ++ "/sources/list")
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson sourceDataDecoder)
        |> send handler


{-| Request source creation.
-}
addSource : { model | credentials : Maybe Credentials, host : String } -> SourceData -> (Result Http.Error Int -> msg) -> Cmd msg
addSource { credentials, host } data handler =
    HttpBuilder.post (host ++ "/source")
        |> withQueryParams (makeAuth credentials)
        |> withJsonBody (sourceDataEncoder data)
        |> withExpect (Http.expectJson affectedIdDecoder)
        |> send handler


{-| Request source update.
-}
updateSource : { model | credentials : Maybe Credentials, host : String } -> SourceData -> (Result Http.Error Int -> msg) -> Cmd msg
updateSource { credentials, host } data handler =
    HttpBuilder.post (host ++ "/source/" ++ toString data.id)
        |> withQueryParams (makeAuth credentials)
        |> withJsonBody (sourceDataEncoder data)
        |> withExpect (Http.expectJson affectedIdDecoder)
        |> send handler


{-| Request source deletion.
-}
deleteSource : { model | credentials : Maybe Credentials, host : String } -> Int -> (Result Http.Error Bool -> msg) -> Cmd msg
deleteSource { credentials, host } id handler =
    HttpBuilder.delete (host ++ "/source/" ++ toString id)
        |> withQueryParams (makeAuth credentials)
        |> withExpect (Http.expectJson successDecoder)
        |> send handler


{-| Request authentication.
-}
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


sourceDataEncoder : SourceData -> Json.Encode.Value
sourceDataEncoder data =
    Json.Encode.object
        [ ( "title", Json.Encode.string data.title )
        , ( "tags", tagsEncoder data.tags )
        , ( "spout", Json.Encode.string data.spout )
        , ( "params", Json.Encode.object (Dict.toList (Dict.map (\_ val -> Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.string val)) data.params)) )
        ]


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


tagsEncoder : List String -> Json.Encode.Value
tagsEncoder tags =
    Json.Encode.string (String.join "," tags)


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


{-| Parse the basic response.

For certain requests, Selfoss responds with either `{"success": true}` or `{"success": false}`.
-}
affectedIdDecoder : Json.Decoder Int
affectedIdDecoder =
    Json.field "success" Json.bool
        |> Json.andThen
            (\success ->
                if success then
                    Json.field "id" intString
                else
                    Json.fail "No source affected"
            )


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
