module Main exposing (main)

{-| This is the main module of the álmafoss RSS reader.

@docs main
-}

import AuthForm
import Api
import Dict exposing (Dict)
import Dom
import Document
import FontAwesome.Web as Icon
import Html exposing (Html, a, article, button, div, h1, h2, header, img, input, li, nav, span, text, ul)
import Html.Keyed as Keyed
import Html.Accessibility.Role as Role exposing (role)
import Html.Accessibility exposing (Tristate(..), ariaChecked, ariaExpanded, ariaLabel, ariaLabelledby, boolToTristate)
import Html.Attributes exposing (alt, class, classList, href, id, src, width, height, tabindex)
import Html.Events exposing (keyCode, onClick)
import Html.Events.Scroll exposing (onScrollToBottom)
import Http
import Keyboard.Combo as Kbd
import Kintail.InputWidget as InputWidget
import List.Extra as List
import Locale exposing (defaultLanguage, supportedLanguages, translate)
import Localization.Language exposing (Language, nativeName)
import Markdown
import Maybe.Extra as Maybe
import Messages
import Navigation exposing (Location)
import RemoteData
import Rocket exposing ((=>))
import Routing
import Scroll
import SourceList
import Task
import Time.DateTime exposing (toISO8601)
import Types exposing (..)
import Utils exposing (onEnter)
import Window


type alias Flags =
    { host : String
    }


type alias Model =
    { host : String
    , lang : Language
    , tags : RemoteData.WebData (List Tag)
    , sources : RemoteData.WebData (List Source)
    , stats : RemoteData.WebData Stats
    , credentials : Maybe Credentials
    , combos : Kbd.Model Msg
    , page : Page
    , route : Routing.Route
    , filtersExpanded : Bool
    , tagsExpanded : Bool
    , sourcesExpanded : Bool
    }


type alias ItemListState =
    { activeItem : Maybe Int
    , filter : Filter
    , items : RemoteData.WebData (List DisplayItem)
    }


type Page
    = ItemList ItemListState
    | SourceList SourceList.Model
    | SignIn AuthForm.Model
    | NotFoundRoute


{-| Entry point of the application.
-}
main : Program Flags Model Msg
main =
    Navigation.programWithFlags OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


initialRoute : Routing.Route
initialRoute =
    Routing.ItemList defaultFilter


init : Flags -> Location -> ( Model, Cmd Msg )
init { host } location =
    let
        route =
            Routing.parseLocation location

        currentPage =
            toPage route

        lang =
            defaultLanguage

        initialModel =
            { host = host
            , lang = lang
            , tags = RemoteData.NotAsked
            , sources = RemoteData.NotAsked
            , stats = RemoteData.NotAsked
            , credentials = Nothing
            , combos = Kbd.init ComboMsg keyboardCombos
            , page = currentPage
            , route = route
            , filtersExpanded = True
            , tagsExpanded = True
            , sourcesExpanded = False
            }
    in
        loadPage Nothing initialModel



-- UPDATE


type Msg
    = Refresh
    | Authenticate
    | Deauthenticate
    | ShowItemList Filter
    | ShowSourceList
    | ItemsResponse (Result Http.Error (List Item))
    | TagsResponse (Result Http.Error (List Tag))
    | SourcesResponse (Result Http.Error (List Source))
    | StatsResponse (Result Http.Error Stats)
    | ForAuthForm AuthForm.Msg
    | ForSourceList SourceList.Msg
    | ComboMsg Kbd.Msg
    | ActivatePrevious
    | ActivateNext
    | ActivateEntry (Maybe Int)
    | SelectPrevious
    | SelectNext
    | ToggleLinkOpen
    | ToggleLinkRead
    | ToggleLinkStarred
    | OpenLink
    | ToggleItemRead Item
    | ToggleItemStarred Item
    | ItemMarkedRead Bool Item
    | ItemMarkedStarred Bool Item
    | MarkingItemReadFailed Bool Item
    | MarkingItemStarredFailed Bool Item
    | LoadMore
    | SelectSource Int
    | SelectTag String
    | SelectAllTags
    | SelectPrimaryFilter FilterPrimary
    | ToggleFiltersExpanded
    | ToggleTagsExpanded
    | ToggleSourcesExpanded
    | ChangeLanguage Language
    | OnLocationChange Location
    | NoOp


setTitle : String -> Cmd Msg
setTitle title =
    Task.perform (always NoOp) (Document.title title)


loadPage : Maybe Routing.Route -> Model -> ( Model, Cmd Msg )
loadPage maybeOldRoute model =
    let
        routeUnchanged =
            Maybe.unwrap False (\oldRoute -> oldRoute == model.route) maybeOldRoute

        page =
            if routeUnchanged then
                model.page
            else
                toPage model.route

        baseModel =
            { model | page = page }
    in
        if routeUnchanged then
            ( baseModel, Cmd.none )
        else
            let
                updateTitle =
                    setTitle (titleFor baseModel page)
            in
                case page of
                    SignIn _ ->
                        let
                            focusUsername =
                                Task.attempt (always NoOp) (Dom.focus "auth-form-username")

                            newCmds =
                                Cmd.batch
                                    [ focusUsername
                                    , updateTitle
                                    ]
                        in
                            ( baseModel, newCmds )

                    ItemList _ ->
                        if Maybe.isJust maybeOldRoute then
                            let
                                newCmds =
                                    Cmd.batch
                                        [ fetchItems baseModel
                                        , updateTitle
                                        ]

                                newModel =
                                    baseModel
                                        |> setItemListItems RemoteData.Loading
                            in
                                ( newModel, newCmds )
                        else
                            let
                                newCmds =
                                    Cmd.batch
                                        [ fetchItems baseModel
                                        , fetchTags baseModel
                                        , fetchSources baseModel
                                        , fetchStats baseModel
                                        , updateTitle
                                        ]

                                newModel =
                                    baseModel
                                        |> setItemListItems RemoteData.Loading
                                        |> setTags RemoteData.Loading
                                        |> setSources RemoteData.Loading
                                        |> setStats RemoteData.Loading
                            in
                                ( newModel, newCmds )

                    SourceList sourceListModel ->
                        let
                            newCmds =
                                Cmd.batch
                                    [ Cmd.map ForSourceList (SourceList.fetchSourceData model.credentials model.host)
                                    , Cmd.map ForSourceList (SourceList.fetchSpouts model.credentials model.host)
                                    , updateTitle
                                    ]

                            newModel =
                                baseModel
                                    |> setSourceListSources RemoteData.Loading
                                    |> setSourceListSpouts RemoteData.Loading
                        in
                            ( newModel, newCmds )

                    NotFoundRoute ->
                        let
                            newCmds =
                                updateTitle
                        in
                            ( baseModel, newCmds )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Refresh ->
            ( setItemListItems RemoteData.Loading model, fetchItems model )

        ItemsResponse result ->
            case result of
                Ok entries ->
                    let
                        newItems =
                            (List.map (\item -> { item = item, open = False }) entries)
                    in
                        ( setItemListItems (RemoteData.Success newItems) model, Cmd.none )

                Err err ->
                    ( setItemListItems (RemoteData.Failure err) model, Cmd.none )

        TagsResponse result ->
            case result of
                Ok tags ->
                    ( setTags (RemoteData.Success tags) model, Cmd.none )

                Err err ->
                    ( setTags (RemoteData.Failure err) model, Cmd.none )

        SourcesResponse result ->
            case result of
                Ok sources ->
                    ( setSources (RemoteData.Success sources) model, Cmd.none )

                Err err ->
                    ( setSources (RemoteData.Failure err) model, Cmd.none )

        StatsResponse result ->
            case result of
                Ok stats ->
                    ( setStats (RemoteData.Success stats) model, Cmd.none )

                Err err ->
                    ( setStats (RemoteData.Failure err) model, Cmd.none )

        Authenticate ->
            ( model, Navigation.newUrl <| Routing.link Routing.SignIn )

        Deauthenticate ->
            ( { model | credentials = Nothing }, Cmd.none )

        ShowItemList filter ->
            ( model, Navigation.newUrl <| Routing.link (Routing.ItemList filter) )

        ShowSourceList ->
            ( model, Navigation.newUrl <| Routing.link Routing.SourceList )

        OpenLink ->
            case model.page of
                ItemList listData ->
                    case listData.items of
                        RemoteData.Success items ->
                            let
                                openCmd =
                                    case listData.activeItem of
                                        Just activeId ->
                                            case getById items activeId of
                                                Just { item } ->
                                                    Task.attempt (always NoOp)
                                                        (Window.open item.url)

                                                Nothing ->
                                                    Cmd.none

                                        Nothing ->
                                            Cmd.none

                                getById items itemId =
                                    items
                                        |> List.find
                                            (\item -> item.item.id == itemId)
                            in
                                ( model, openCmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectPrevious ->
            case model.page of
                ItemList listData ->
                    case listData.items of
                        RemoteData.Success items ->
                            let
                                previousItem =
                                    findPrevious (List.map .item items) listData.activeItem

                                scrollCmd =
                                    case previousItem of
                                        Just activeId ->
                                            Task.attempt (always NoOp)
                                                (Scroll.intoView ("entry-" ++ toString activeId))

                                        Nothing ->
                                            Cmd.none
                            in
                                ( setItemListActiveItem previousItem model, scrollCmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectNext ->
            case model.page of
                ItemList listData ->
                    case listData.items of
                        RemoteData.Success items ->
                            let
                                nextItem =
                                    findNext (List.map .item items) listData.activeItem

                                scrollCmd =
                                    case nextItem of
                                        Just activeId ->
                                            Task.attempt (always NoOp)
                                                (Scroll.intoView ("entry-" ++ toString activeId))

                                        Nothing ->
                                            Cmd.none
                            in
                                ( setItemListActiveItem nextItem model, scrollCmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ActivatePrevious ->
            case model.page of
                ItemList listData ->
                    case listData.items of
                        RemoteData.Success items ->
                            let
                                previousItem =
                                    findPrevious (List.map .item items) listData.activeItem

                                scrollCmd =
                                    case previousItem of
                                        Just activeId ->
                                            Task.attempt (always NoOp)
                                                (Scroll.intoView ("entry-" ++ toString activeId))

                                        Nothing ->
                                            Cmd.none

                                newItems =
                                    items
                                        |> List.map
                                            (\item ->
                                                if Just item.item.id == listData.activeItem then
                                                    { item | open = False }
                                                else if Just item.item.id == previousItem then
                                                    { item | open = True }
                                                else
                                                    item
                                            )

                                newModel =
                                    model
                                        |> setItemListItems (RemoteData.Success newItems)
                                        |> setItemListActiveItem previousItem
                            in
                                ( newModel, scrollCmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ActivateNext ->
            case model.page of
                ItemList listData ->
                    case listData.items of
                        RemoteData.Success items ->
                            let
                                nextItem =
                                    findNext (List.map .item items) listData.activeItem

                                scrollCmd =
                                    case nextItem of
                                        Just activeId ->
                                            Task.attempt (always NoOp)
                                                (Scroll.intoView ("entry-" ++ toString activeId))

                                        Nothing ->
                                            Cmd.none

                                newItems =
                                    items
                                        |> List.map
                                            (\item ->
                                                if Just item.item.id == listData.activeItem then
                                                    { item | open = False }
                                                else if Just item.item.id == nextItem then
                                                    { item | open = True }
                                                else
                                                    item
                                            )

                                newModel =
                                    model
                                        |> setItemListItems (RemoteData.Success newItems)
                                        |> setItemListActiveItem nextItem
                            in
                                ( newModel, scrollCmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ActivateEntry itemId ->
            case model.page of
                ItemList listData ->
                    case listData.items of
                        RemoteData.Success items ->
                            let
                                newItems =
                                    items
                                        |> List.map
                                            (\item ->
                                                if Just item.item.id == itemId then
                                                    { item | open = not item.open }
                                                else
                                                    item
                                            )

                                newModel =
                                    model
                                        |> setItemListItems (RemoteData.Success newItems)
                                        |> setItemListActiveItem itemId
                            in
                                ( newModel, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleLinkOpen ->
            case model.page of
                ItemList listData ->
                    case listData.items of
                        RemoteData.Success items ->
                            let
                                upd item =
                                    { item | open = not item.open }

                                newItems =
                                    List.updateIf (\item -> Just item.item.id == listData.activeItem) upd items

                                newModel =
                                    model
                                        |> setItemListItems (RemoteData.Success newItems)
                            in
                                ( newModel, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleLinkRead ->
            case model.page of
                ItemList listData ->
                    case listData.items of
                        RemoteData.Success items ->
                            case List.find (\item -> Just item.item.id == listData.activeItem) items of
                                Just { item } ->
                                    update (ToggleItemRead item) model

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleLinkStarred ->
            case model.page of
                ItemList listData ->
                    case listData.items of
                        RemoteData.Success items ->
                            case List.find (\item -> Just item.item.id == listData.activeItem) items of
                                Just { item } ->
                                    update (ToggleItemStarred item) model

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleItemStarred item ->
            let
                upd item =
                    { item | starred = not item.starred }

                action =
                    if item.starred then
                        unstarItem model item
                    else
                        starItem model item
            in
                ( updateItem item.id upd model, action )

        ToggleItemRead item ->
            let
                upd item =
                    { item | unread = not item.unread }

                action =
                    if item.unread then
                        markItemRead model item
                    else
                        markItemUnread model item
            in
                ( updateItem item.id upd model, action )

        ItemMarkedRead val item ->
            ( model, Cmd.none )

        ItemMarkedStarred val item ->
            ( model, Cmd.none )

        MarkingItemReadFailed val item ->
            let
                upd item =
                    { item | unread = val }
            in
                ( updateItem item.id upd model, Cmd.none )

        MarkingItemStarredFailed val item ->
            let
                upd item =
                    { item | unread = val }
            in
                ( updateItem item.id upd model, Cmd.none )

        LoadMore ->
            case model.page of
                ItemList listData ->
                    case listData.items of
                        RemoteData.Success items ->
                            let
                                nextItems =
                                    findPrevious (List.map .item items) listData.activeItem
                            in
                                model
                                    |> setItemListItems RemoteData.Loading
                                    => Cmd.none

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectTag tagName ->
            let
                newFilter =
                    case model.page of
                        ItemList listData ->
                            let
                                oldFilter =
                                    listData.filter
                            in
                                { oldFilter | secondary = OnlyTag tagName }

                        _ ->
                            defaultFilter
            in
                update (ShowItemList newFilter) model

        SelectAllTags ->
            let
                newFilter =
                    case model.page of
                        ItemList listData ->
                            let
                                oldFilter =
                                    listData.filter
                            in
                                { oldFilter | secondary = AllTags }

                        _ ->
                            defaultFilter
            in
                update (ShowItemList newFilter) model

        SelectSource sourceId ->
            let
                newFilter =
                    case model.page of
                        ItemList listData ->
                            let
                                oldFilter =
                                    listData.filter
                            in
                                { oldFilter | secondary = OnlySource sourceId }

                        _ ->
                            defaultFilter
            in
                update (ShowItemList newFilter) model

        SelectPrimaryFilter filter ->
            let
                newFilter =
                    case model.page of
                        ItemList listData ->
                            let
                                oldFilter =
                                    listData.filter
                            in
                                { oldFilter | primary = filter }

                        _ ->
                            defaultFilter
            in
                update (ShowItemList newFilter) model

        ForAuthForm (AuthForm.AuthSucceeded credentials) ->
            let
                newModel =
                    { model | credentials = Just credentials }

                cmd =
                    Navigation.newUrl <| Routing.link initialRoute
            in
                ( newModel, cmd )

        ForAuthForm (AuthForm.Authenticate) ->
            case model.page of
                SignIn authFormModel ->
                    let
                        cmd =
                            AuthForm.authenticate authFormModel.credentials model.host
                    in
                        ( model, Cmd.map ForAuthForm cmd )

                _ ->
                    ( model, Cmd.none )

        ForAuthForm msg ->
            case model.page of
                SignIn authFormModel ->
                    let
                        newModel =
                            AuthForm.update msg authFormModel
                    in
                        ( setAuthFormModel newModel model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ForSourceList msg ->
            case model.page of
                SourceList sourceListModel ->
                    let
                        ( newModel, cmd ) =
                            SourceList.update model.credentials model.host msg sourceListModel
                    in
                        ( setSourceListModel newModel model, Cmd.map ForSourceList cmd )

                _ ->
                    ( model, Cmd.none )

        ComboMsg msg ->
            let
                updatedCombos =
                    Kbd.update msg model.combos
            in
                ( { model | combos = updatedCombos }, Cmd.none )

        ToggleFiltersExpanded ->
            ( { model | filtersExpanded = not model.filtersExpanded }, Cmd.none )

        ToggleTagsExpanded ->
            ( { model | tagsExpanded = not model.tagsExpanded }, Cmd.none )

        ToggleSourcesExpanded ->
            ( { model | sourcesExpanded = not model.sourcesExpanded }, Cmd.none )

        ChangeLanguage lang ->
            ( { model | lang = lang }, Cmd.none )

        OnLocationChange location ->
            let
                route =
                    Routing.parseLocation location

                newModel =
                    { model | route = route }
            in
                loadPage (Just model.route) newModel

        NoOp ->
            ( model, Cmd.none )


titleFor : Model -> Page -> String
titleFor model page =
    let
        tr =
            translate model.lang

        makeTitle msg =
            tr (Messages.Title (Just (tr msg)))
    in
        case page of
            ItemList _ ->
                makeTitle Messages.ItemListTitle

            SourceList _ ->
                makeTitle Messages.SourceListTitle

            SignIn _ ->
                makeTitle Messages.AuthenticationTitle

            NotFoundRoute ->
                makeTitle Messages.NotFoundTitle


keyboardCombos : List (Kbd.KeyCombo Msg)
keyboardCombos =
    [ Kbd.combo1 Kbd.r Refresh
    , Kbd.combo1 Kbd.k ActivatePrevious
    , Kbd.combo1 Kbd.j ActivateNext
    , Kbd.combo1 Kbd.p SelectPrevious
    , Kbd.combo1 Kbd.n SelectNext
    , Kbd.combo1 Kbd.v OpenLink
    , Kbd.combo1 Kbd.o ToggleLinkOpen
    , Kbd.combo1 Kbd.m ToggleLinkRead
    , Kbd.combo1 Kbd.s ToggleLinkStarred
    ]


findPrevious : List Item -> Maybe Int -> Maybe Int
findPrevious items activeItem =
    case activeItem of
        Just activeId ->
            case List.head items of
                Just h ->
                    if h.id == activeId then
                        Just h.id
                    else
                        let
                            findPrevious_ items_ =
                                case items_ of
                                    x :: y :: xs ->
                                        if y.id == activeId then
                                            Just x.id
                                        else
                                            findPrevious_ (y :: xs)

                                    _ ->
                                        Nothing
                        in
                            findPrevious_ items

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


findNext : List Item -> Maybe Int -> Maybe Int
findNext items activeItem =
    case activeItem of
        Just activeId ->
            let
                findNext_ items_ =
                    case items_ of
                        x :: y :: xs ->
                            if x.id == activeId then
                                Just y.id
                            else
                                findNext_ (y :: xs)

                        [ x ] ->
                            if x.id == activeId then
                                Just x.id
                            else
                                Nothing

                        [] ->
                            Nothing
            in
                findNext_ items

        Nothing ->
            Maybe.map .id (List.head items)



-- VIEW


logo : Html Msg
logo =
    img [ class "logo", src "assets/logo.png", alt "", role Role.Presentation ] []


sidebar : Model -> Html Msg
sidebar model =
    div [ class "sidebar" ]
        [ header [ role Role.Banner ]
            [ logo
            , h1 [] [ text <| translate model.lang Messages.AppName ]
            ]
        , nav [ class "nav-content" ]
            [ navFilters model
            , navTags model
            , navSources model
            ]
        , navActions model
        ]


unreadBadge : Model -> Int -> Html Msg
unreadBadge model unread =
    if unread > 0 then
        span [ class "unread", ariaLabel (translate model.lang (Messages.NumberUnread unread)) ] [ text (toString unread) ]
    else
        span [] []


navCollapsible : String -> Html Msg -> (key -> Msg) -> Maybe key -> RemoteData.WebData (List ( key, Html Msg )) -> Bool -> Msg -> Html Msg
navCollapsible widgetId label action activeKey webItems expanded toggleExpanded =
    case webItems of
        RemoteData.NotAsked ->
            --Debug.crash "Trying to show list and did not ask for data."
            text "NotAsked"

        RemoteData.Loading ->
            text "Loading"

        RemoteData.Failure err ->
            text (toString err)

        RemoteData.Success items ->
            let
                labelId =
                    widgetId ++ "-label"

                item ( key, label ) =
                    let
                        active =
                            Just key == activeKey
                    in
                        ( toString key, li [ onClick (action key), onEnter (action key), classList [ ( "active", active ) ], role Role.MenuItemRadio, ariaChecked (Just (boolToTristate active)) ] [ label ] )
            in
                div [ class widgetId ]
                    [ h2 [ id labelId, ariaExpanded (Just expanded), onClick toggleExpanded, onEnter toggleExpanded, tabindex 0 ] [ label ]
                    , Keyed.ul [ role Role.Group, ariaLabelledby [ labelId ], id (widgetId ++ "-content"), classList [ ( "collapsed", not expanded ) ] ] (List.map item items)
                    ]


navFilters : Model -> Html Msg
navFilters model =
    let
        filters =
            let
                filterLabel ( key, message, stat ) =
                    ( key, span [] [ text <| translate model.lang message, RemoteData.withDefault (text "") (RemoteData.map (unreadBadge model << stat) model.stats) ] )
            in
                List.map filterLabel [ ( AllItems, Messages.All, .total ), ( UnreadItems, Messages.Unread, .unread ), ( StarredItems, Messages.Starred, .starred ) ]

        activePrimaryFilter =
            case model.page of
                ItemList { filter } ->
                    Just filter.primary

                _ ->
                    Nothing
    in
        navCollapsible "nav-filters"
            (text <| translate model.lang Messages.Filters)
            SelectPrimaryFilter
            activePrimaryFilter
            (RemoteData.Success filters)
            model.filtersExpanded
            ToggleFiltersExpanded


navTags : Model -> Html Msg
navTags model =
    let
        activeTag =
            case model.page of
                ItemList { filter } ->
                    case filter.secondary of
                        OnlyTag _ ->
                            Just filter.secondary

                        AllTags ->
                            Just filter.secondary

                        _ ->
                            Nothing

                _ ->
                    Nothing

        toAction filter =
            case filter of
                OnlyTag tagName ->
                    SelectTag tagName

                AllTags ->
                    SelectAllTags

                _ ->
                    NoOp

        tags tags =
            let
                tagLabel tag =
                    ( OnlyTag tag.tag, span [] [ text tag.tag, unreadBadge model tag.unread, span [ class "tag-color", Html.Attributes.style [ ( "background-color", tag.color ) ] ] [] ] )
            in
                ( AllTags, text <| translate model.lang Messages.AllTags ) :: List.map tagLabel tags
    in
        navCollapsible "nav-tags"
            (text <| translate model.lang Messages.Tags)
            toAction
            activeTag
            (RemoteData.map tags model.tags)
            model.tagsExpanded
            ToggleTagsExpanded


navSources : Model -> Html Msg
navSources model =
    let
        activeSourceId =
            case model.page of
                ItemList { filter } ->
                    case filter.secondary of
                        OnlySource sourceId ->
                            Just sourceId

                        _ ->
                            Nothing

                _ ->
                    Nothing

        sources sources =
            let
                sourceLabel source =
                    ( source.id, span [] [ text source.title, unreadBadge model source.unread ] )
            in
                List.map sourceLabel sources
    in
        navCollapsible "nav-sources"
            (text <| translate model.lang Messages.Sources)
            SelectSource
            activeSourceId
            (RemoteData.map sources model.sources)
            model.sourcesExpanded
            ToggleSourcesExpanded


navActions : Model -> Html Msg
navActions model =
    let
        debugButtons =
            [ button [ onClick Refresh ]
                [ Icon.refresh
                , text " "
                , text <| translate model.lang Messages.Refresh
                ]
            ]

        languageCombo =
            [ Html.map ChangeLanguage <| InputWidget.comboBox [] nativeName supportedLanguages model.lang ]

        authButton =
            if Maybe.isJust model.credentials then
                [ button [ onClick Deauthenticate ]
                    [ Icon.sign_out
                    , text " "
                    , text <| translate model.lang Messages.SignOut
                    ]
                ]
            else
                [ button [ onClick Authenticate ]
                    [ Icon.sign_in
                    , text " "
                    , text <| translate model.lang Messages.SignIn
                    ]
                ]

        sourcesButton =
            if Maybe.isJust model.credentials then
                [ button [ onClick ShowSourceList ]
                    [ Icon.cloud_upload, text " ", text <| translate model.lang Messages.ManageSources ]
                ]
            else
                []
    in
        nav [ class "nav-action" ]
            (sourcesButton ++ authButton ++ [ h2 [] [ text "Debug" ] ] ++ debugButtons ++ [ Html.br [] [] ] ++ languageCombo)


getTagColor : Model -> String -> String
getTagColor model tag =
    model.tags
        |> RemoteData.toMaybe
        |> Maybe.andThen (List.find (\t -> t.tag == tag))
        |> Maybe.map .color
        |> Maybe.withDefault "#cccccc"


tag : Model -> String -> Html Msg
tag model t =
    span
        [ class "tag"
        , onClick (SelectTag t)
        , Html.Attributes.style [ ( "background-color", getTagColor model t ) ]
        ]
        [ text t ]


entry : Model -> Maybe Int -> DisplayItem -> Html Msg
entry model activeId { item, open } =
    let
        author =
            case item.author of
                Just author ->
                    if String.isEmpty author then
                        []
                    else
                        [ text author ]

                Nothing ->
                    []

        source =
            span [ class "entry-source", onClick (SelectSource item.source.id) ] [ text item.source.title ]

        active =
            Just item.id == activeId
    in
        article [ id ("entry-" ++ toString item.id), classList [ ( "entry", True ), ( "active", active ), ( "unread", item.unread ), ( "open", open ) ], ariaExpanded (Just open) ]
            [ header [ class "entry-header" ]
                (let
                    icon =
                        [ img [ src (model.host ++ "/data/favicons/" ++ item.icon), alt "", width 16, height 16, class "entry-icon" ] [] ]

                    title =
                        [ h1 [ onClick (ActivateEntry (Just item.id)) ] [ text item.title ] ]

                    tags =
                        [ span [ class "entry-tags" ] (List.map (tag model) item.tags) ]

                    info =
                        List.intersperse (text " · ")
                            ([ source ] ++ author ++ [ a [ href item.url ] [ text <| toISO8601 item.datetime ] ])
                 in
                    icon ++ title ++ tags ++ info
                )
            , div [ class "entry-content" ] [ Markdown.toHtml [] item.content ]
            , entryPanel model item
            ]


entryPanel : Model -> Item -> Html Msg
entryPanel model item =
    let
        readButton =
            if Maybe.isJust model.credentials then
                [ button [ onClick (ToggleItemRead item) ]
                    [ if item.unread then
                        Icon.check_circle_o
                      else
                        Icon.check_circle
                    , text " "
                    , text <|
                        translate model.lang
                            (if item.unread then
                                Messages.MarkEntryRead
                             else
                                Messages.MarkEntryUnread
                            )
                    ]
                ]
            else
                []

        starButton =
            if Maybe.isJust model.credentials then
                [ button [ onClick (ToggleItemStarred item) ]
                    [ if item.starred then
                        Icon.star
                      else
                        Icon.star_o
                    , text " "
                    , text <|
                        translate model.lang
                            (if item.starred then
                                Messages.UnstarEntry
                             else
                                Messages.StarEntry
                            )
                    ]
                ]
            else
                []
    in
        div [ class "entry-panel" ]
            (readButton ++ starButton)


view : Model -> Html Msg
view model =
    let
        evts =
            case model.page of
                ItemList listData ->
                    if RemoteData.isSuccess listData.items then
                        [ onScrollToBottom LoadMore ]
                    else
                        []

                _ ->
                    []
    in
        div [ class "almafoss", Html.Attributes.lang (Localization.Language.code model.lang) ]
            [ sidebar model
            , Html.main_ evts [ mainContent model ]
            ]


mainContent : Model -> Html Msg
mainContent model =
    case model.page of
        SignIn authFormModel ->
            AuthForm.authForm ForAuthForm authFormModel model.lang

        ItemList listData ->
            case listData.items of
                RemoteData.Loading ->
                    text <| translate model.lang Messages.Loading

                RemoteData.NotAsked ->
                    Debug.crash "Trying to show list of items and did not ask for data."

                RemoteData.Success items ->
                    if List.isEmpty items then
                        text <| translate model.lang Messages.NoEntries
                    else
                        let
                            entries =
                                List.map (entry model listData.activeItem) items
                        in
                            div [ class "entries", role Role.Feed ] entries

                RemoteData.Failure err ->
                    case err of
                        Http.BadStatus { status } ->
                            if status.code == 403 then
                                text <| translate model.lang Messages.AuthenticationRequired
                            else
                                Markdown.toHtml [] (toString err)

                        _ ->
                            Markdown.toHtml [] (toString err)

        SourceList sourceListModel ->
            Html.map ForSourceList (SourceList.sourceList model.host sourceListModel model.lang)

        NotFoundRoute ->
            text <| translate model.lang Messages.NotFound



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Kbd.subscriptions model.combos
        ]



-- SETTERS


setSources : RemoteData.WebData (List Source) -> Model -> Model
setSources sources model =
    { model | sources = sources }


setTags : RemoteData.WebData (List Tag) -> Model -> Model
setTags tags model =
    { model | tags = tags }


setStats : RemoteData.WebData Stats -> Model -> Model
setStats stats model =
    { model | stats = stats }


setItemListItems : RemoteData.WebData (List DisplayItem) -> Model -> Model
setItemListItems items =
    mapItemListItems (always items)


setSourceListSources : RemoteData.WebData (List SourceList.DisplaySourceData) -> Model -> Model
setSourceListSources sources =
    mapSourceListSources (always sources)


setSourceListSpouts : RemoteData.WebData (Dict String Spout) -> Model -> Model
setSourceListSpouts spouts =
    mapSourceListSpouts (always spouts)


setSourceListModel : SourceList.Model -> Model -> Model
setSourceListModel model =
    mapPage (mapSourceListModel (always model))


setAuthFormModel : AuthForm.Model -> Model -> Model
setAuthFormModel model =
    mapPage (mapAuthFormModel (always model))


setItemListActiveItem : Maybe Int -> Model -> Model
setItemListActiveItem activeItem =
    mapItemListActiveItem (always activeItem)


mapPage : Mapper Model Page
mapPage f data =
    { data | page = f data.page }


mapItemListState : Mapper Page ItemListState
mapItemListState f data =
    case data of
        ItemList listData ->
            ItemList (f listData)

        _ ->
            data


mapSources : Mapper SourceList.Model (RemoteData.WebData (List SourceList.DisplaySourceData))
mapSources f data =
    { data | sources = f data.sources }


mapSpouts : Mapper SourceList.Model (RemoteData.WebData (Dict String Spout))
mapSpouts f data =
    { data | spouts = f data.spouts }


mapSourceListModel : Mapper Page SourceList.Model
mapSourceListModel f data =
    case data of
        SourceList model ->
            SourceList (f model)

        _ ->
            data


mapAuthFormModel : Mapper Page AuthForm.Model
mapAuthFormModel f data =
    case data of
        SignIn model ->
            SignIn (f model)

        _ ->
            data


mapItems : Mapper ItemListState (RemoteData.WebData (List DisplayItem))
mapItems f data =
    { data | items = f data.items }


mapActiveItem : Mapper ItemListState (Maybe Int)
mapActiveItem f data =
    { data | activeItem = f data.activeItem }


mapItemListItems : Mapper Model (RemoteData.WebData (List DisplayItem))
mapItemListItems =
    mapPage << mapItemListState << mapItems


mapSourceListSources : Mapper Model (RemoteData.WebData (List SourceList.DisplaySourceData))
mapSourceListSources =
    mapPage << mapSourceListModel << mapSources


mapSourceListSpouts : Mapper Model (RemoteData.WebData (Dict String Spout))
mapSourceListSpouts =
    mapPage << mapSourceListModel << mapSpouts


mapItemListActiveItem : Mapper Model (Maybe Int)
mapItemListActiveItem =
    mapPage << mapItemListState << mapActiveItem



-- HTTP


fetchItems : Model -> Cmd Msg
fetchItems model =
    let
        filter =
            case model.page of
                ItemList { filter } ->
                    filter

                _ ->
                    defaultFilter
    in
        Api.items
            model
            filter
            Nothing
            ItemsResponse


fetchTags : Model -> Cmd Msg
fetchTags model =
    Api.tags
        model
        TagsResponse


fetchStats : Model -> Cmd Msg
fetchStats model =
    Api.stats
        model
        StatsResponse


fetchSources : Model -> Cmd Msg
fetchSources model =
    Api.sources
        model
        SourcesResponse


markItemRead : Model -> Item -> Cmd Msg
markItemRead model item =
    Api.markItemRead
        model
        item
        (\res ->
            case res of
                Ok _ ->
                    ItemMarkedRead True item

                Err _ ->
                    MarkingItemReadFailed True item
        )


markItemUnread : Model -> Item -> Cmd Msg
markItemUnread model item =
    Api.markItemUnread
        model
        item
        (\res ->
            case res of
                Ok _ ->
                    ItemMarkedRead False item

                Err _ ->
                    MarkingItemReadFailed False item
        )


starItem : Model -> Item -> Cmd Msg
starItem model item =
    Api.starItem
        model
        item
        (\res ->
            case res of
                Ok _ ->
                    ItemMarkedStarred True item

                Err _ ->
                    MarkingItemStarredFailed True item
        )


unstarItem : Model -> Item -> Cmd Msg
unstarItem model item =
    Api.unstarItem
        model
        item
        (\res ->
            case res of
                Ok _ ->
                    ItemMarkedStarred False item

                Err _ ->
                    MarkingItemStarredFailed False item
        )


updateItem : Int -> (Item -> Item) -> Model -> Model
updateItem itemId upd =
    mapItemListItems
        (RemoteData.map
            (List.map
                (\item ->
                    if item.item.id == itemId then
                        { item | item = upd item.item }
                    else
                        item
                )
            )
        )


toPage : Routing.Route -> Page
toPage route =
    case route of
        Routing.ItemList filter ->
            ItemList (ItemListState Nothing filter RemoteData.NotAsked)

        Routing.SourceList ->
            SourceList SourceList.init

        Routing.SignIn ->
            SignIn (AuthForm.init)

        Routing.NotFoundRoute ->
            NotFoundRoute
