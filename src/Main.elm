module Main exposing (main)

{-| This is the main module of the álmafoss RSS reader.

@docs main
-}

import AuthForm
import Api
import Dom
import Document
import FontAwesome.Web as Icon
import Html exposing (Html, a, article, button, div, h1, h2, header, img, input, li, nav, span, text, ul)
import Html.Keyed as Keyed
import Html.Accessibility.Role as Role exposing (role)
import Html.Accessibility exposing (Tristate(..), ariaChecked, ariaExpanded, ariaLabel, ariaLabelledby, boolToTristate)
import Html.Attributes exposing (alt, class, classList, href, id, src, width, height, tabindex)
import Html.Events exposing (keyCode, onClick)
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
import Routing
import Scroll
import SourceList
import Task
import Types exposing (..)
import Utils exposing (onEnter)
import Window


type alias Flags =
    { host : String
    }


type alias DisplayItem =
    { item : Item
    , open : Bool
    }


type alias Model =
    { host : String
    , lang : Language
    , title : String
    , error : Maybe String
    , items : List DisplayItem
    , tags : List Tag
    , sources : List Source
    , sourceList : SourceList.Model
    , stats : Stats
    , credentials : Maybe Credentials
    , authForm : AuthForm.Model
    , combos : Kbd.Model Msg
    , route : Routing.Route
    , filtersExpanded : Bool
    , tagsExpanded : Bool
    , sourcesExpanded : Bool
    , loading : Bool
    }


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
    Routing.ItemList { activeItem = Nothing, filter = defaultFilter }


init : Flags -> Location -> ( Model, Cmd Msg )
init { host } location =
    let
        currentRoute =
            Routing.parseLocation location

        lang = defaultLanguage

        (sourceListModel, sourceListCmds) = SourceList.init host lang Nothing

        initialModel =
            { host = host
            , lang = lang
            , title = "álmafoss"
            , error = Nothing
            , items = []
            , tags = []
            , sources = []
            , sourceList = sourceListModel
            , stats = Stats 0 0 0
            , credentials = Nothing
            , authForm = AuthForm.initModel host defaultLanguage
            , combos = Kbd.init ComboMsg keyboardCombos
            , filtersExpanded = True
            , tagsExpanded = True
            , sourcesExpanded = False
            , loading = False
            , route = currentRoute
            }
    in
        ( initialModel
        , Cmd.batch (Cmd.map SourceList sourceListCmds :: extraCmds Nothing initialModel)
        )



-- UPDATE


type Msg
    = Refresh
    | RefreshTags
    | RefreshSources
    | Authenticate
    | Deauthenticate
    | ShowItemList Filter
    | ShowSourceList
    | FetchingFailed Http.Error
    | ItemsFetched (List Item)
    | TagsFetched (List Tag)
    | SourcesFetched (List Source)
    | StatsFetched Stats
    | AuthForm AuthForm.Msg
    | SourceList SourceList.Msg
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


extraCmds : Maybe Model -> Model -> List (Cmd Msg)
extraCmds maybeOldModel newModel =
    let
        stripActiveId route =
            case route of
                Routing.ItemList listData ->
                    Routing.ItemList { listData | activeItem = Nothing }

                _ ->
                    route

        routeChanged =
            case maybeOldModel of
                Just oldModel ->
                    stripActiveId oldModel.route == stripActiveId newModel.route

                Nothing ->
                    False
    in
        if routeChanged then
            [ Cmd.none ]
        else
            case newModel.route of
                Routing.AuthError ->
                    [ Task.attempt (always NoOp) (Dom.focus "auth-form-username") ]

                Routing.ItemList _ ->
                    [ fetchItems newModel
                    , fetchTags newModel
                    , fetchSources newModel
                    , fetchStats newModel
                    ]

                _ ->
                    [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Refresh ->
            ( { model | loading = True }, fetchItems model )

        RefreshTags ->
            ( model, fetchTags model )

        RefreshSources ->
            ( model, fetchSources model )

        ItemsFetched entries ->
            ( { model
                | loading = False
                , items = List.map (\item -> { item = item, open = False }) entries
              }
            , Cmd.none
            )

        --SourceDataFetched data ->
        --    ( { model
        --        | loading = False
        --      }
        --    , Cmd.none
        --    )

        TagsFetched tags ->
            ( { model | tags = tags }, Cmd.none )

        SourcesFetched sources ->
            ( { model | sources = sources }, Cmd.none )

        StatsFetched stats ->
            ( { model | stats = stats }, Cmd.none )

        FetchingFailed err ->
            ( { model
                | error =
                    Just <|
                        case err of
                            Http.BadStatus { status } ->
                                if status.code == 403 then
                                    translate model.lang Messages.AuthenticationRequired
                                else
                                    toString err

                            _ ->
                                toString err
              }
            , Cmd.none
            )

        Authenticate ->
            ( model, Navigation.newUrl <| Routing.link Routing.AuthError )

        Deauthenticate ->
            ( { model | credentials = Nothing }, Cmd.none )

        ShowItemList filter ->
            let
                newModel =
                    { model | loading = True }

                cmds =
                    Cmd.batch
                        [ Navigation.newUrl <| Routing.link (Routing.ItemList { activeItem = Nothing, filter = filter })
                        , fetchItems model
                        ]
            in
                ( newModel, cmds )

        ShowSourceList ->
            let
                newModel =
                    { model | loading = True }

                cmds =
                    Cmd.batch
                        [ Navigation.newUrl <| Routing.link Routing.SourceList
                          -- , fetchSources model
                        ]
            in
                ( newModel, cmds )

        AuthForm (AuthForm.AuthSucceeded credentials) ->
            let
                newModel =
                    { model
                        | credentials = Just credentials
                    }

                cmds =
                    Cmd.batch
                        [ fetchItems newModel
                        , Navigation.newUrl <| Routing.link initialRoute
                        ]
            in
                ( newModel
                , cmds
                )

        OpenLink ->
            case model.route of
                Routing.ItemList listData ->
                    let
                        openCmd =
                            case listData.activeItem of
                                Just activeId ->
                                    case getById model.items activeId of
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

        SelectPrevious ->
            case model.route of
                Routing.ItemList listData ->
                    let
                        previousItem =
                            findPrevious (List.map .item model.items) listData.activeItem

                        scrollCmd =
                            case previousItem of
                                Just activeId ->
                                    Task.attempt (always NoOp)
                                        (Scroll.intoView ("entry-" ++ toString activeId))

                                Nothing ->
                                    Cmd.none

                        cmds =
                            Cmd.batch
                                [ Navigation.newUrl <| Routing.link (Routing.ItemList { listData | activeItem = previousItem })
                                , scrollCmd
                                ]
                    in
                        ( model, cmds )

                _ ->
                    ( model, Cmd.none )

        SelectNext ->
            case model.route of
                Routing.ItemList listData ->
                    let
                        nextItem =
                            findNext (List.map .item model.items) listData.activeItem

                        scrollCmd =
                            case nextItem of
                                Just activeId ->
                                    Task.attempt (always NoOp)
                                        (Scroll.intoView ("entry-" ++ toString activeId))

                                Nothing ->
                                    Cmd.none

                        cmds =
                            Cmd.batch
                                [ Navigation.newUrl <| Routing.link (Routing.ItemList { listData | activeItem = nextItem })
                                , scrollCmd
                                ]
                    in
                        ( model, cmds )

                _ ->
                    ( model, Cmd.none )

        ActivatePrevious ->
            case model.route of
                Routing.ItemList listData ->
                    let
                        previousItem =
                            findPrevious (List.map .item model.items) listData.activeItem

                        scrollCmd =
                            case previousItem of
                                Just activeId ->
                                    Task.attempt (always NoOp)
                                        (Scroll.intoView ("entry-" ++ toString activeId))

                                Nothing ->
                                    Cmd.none

                        newItems =
                            model.items
                                |> List.map
                                    (\item ->
                                        if Just item.item.id == listData.activeItem then
                                            { item | open = False }
                                        else if Just item.item.id == previousItem then
                                            { item | open = True }
                                        else
                                            item
                                    )

                        cmds =
                            Cmd.batch
                                [ Navigation.newUrl <| Routing.link (Routing.ItemList { listData | activeItem = previousItem })
                                , scrollCmd
                                ]
                    in
                        ( { model | items = newItems }, cmds )

                _ ->
                    ( model, Cmd.none )

        ActivateNext ->
            case model.route of
                Routing.ItemList listData ->
                    let
                        nextItem =
                            findNext (List.map .item model.items) listData.activeItem

                        scrollCmd =
                            case nextItem of
                                Just activeId ->
                                    Task.attempt (always NoOp)
                                        (Scroll.intoView ("entry-" ++ toString activeId))

                                Nothing ->
                                    Cmd.none

                        newItems =
                            model.items
                                |> List.map
                                    (\item ->
                                        if Just item.item.id == listData.activeItem then
                                            { item | open = False }
                                        else if Just item.item.id == nextItem then
                                            { item | open = True }
                                        else
                                            item
                                    )

                        cmds =
                            Cmd.batch
                                [ Navigation.newUrl <| Routing.link (Routing.ItemList { listData | activeItem = nextItem })
                                , scrollCmd
                                ]
                    in
                        ( { model | items = newItems }, cmds )

                _ ->
                    ( model, Cmd.none )

        ActivateEntry itemId ->
            case model.route of
                Routing.ItemList listData ->
                    let
                        newItems =
                            model.items
                                |> List.map
                                    (\item ->
                                        if Just item.item.id == itemId then
                                            { item | open = not item.open }
                                        else
                                            item
                                    )

                        cmds =
                            Cmd.batch
                                [ Navigation.newUrl <| Routing.link (Routing.ItemList { listData | activeItem = itemId })
                                ]
                    in
                        ( { model | items = newItems }, cmds )

                _ ->
                    ( model, Cmd.none )

        ToggleLinkOpen ->
            case model.route of
                Routing.ItemList listData ->
                    let
                        upd item =
                            { item | open = not item.open }

                        newItems =
                            List.updateIf (\item -> Just item.item.id == listData.activeItem) upd model.items
                    in
                        ( { model | items = newItems }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleLinkRead ->
            case model.route of
                Routing.ItemList listData ->
                    case List.find (\item -> Just item.item.id == listData.activeItem) model.items of
                        Just { item } ->
                            update (ToggleItemRead item) model

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleLinkStarred ->
            case model.route of
                Routing.ItemList listData ->
                    case List.find (\item -> Just item.item.id == listData.activeItem) model.items of
                        Just { item } ->
                            update (ToggleItemStarred item) model

                        Nothing ->
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
                ( { model | items = updateItems item.id upd model.items }, action )

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
                ( { model | items = updateItems item.id upd model.items }, action )

        ItemMarkedRead val item ->
            ( model, Cmd.none )

        ItemMarkedStarred val item ->
            ( model, Cmd.none )

        MarkingItemReadFailed val item ->
            let
                upd item =
                    { item | unread = val }
            in
                ( { model | items = updateItems item.id upd model.items }, Cmd.none )

        MarkingItemStarredFailed val item ->
            let
                upd item =
                    { item | unread = val }
            in
                ( { model | items = updateItems item.id upd model.items }, Cmd.none )

        SelectTag tagName ->
            let
                newFilter =
                    case model.route of
                        Routing.ItemList listData ->
                            let
                                oldFilter =
                                    listData.filter
                            in
                                { oldFilter | secondary = OnlyTag tagName }

                        _ ->
                            defaultFilter

                newModel =
                    changeFilter newFilter model
            in
                update (ShowItemList newFilter) newModel

        SelectAllTags ->
            let
                newFilter =
                    case model.route of
                        Routing.ItemList listData ->
                            let
                                oldFilter =
                                    listData.filter
                            in
                                { oldFilter | secondary = AllTags }

                        _ ->
                            defaultFilter

                newModel =
                    changeFilter newFilter model
            in
                update (ShowItemList newFilter) newModel

        SelectSource sourceId ->
            let
                newFilter =
                    case model.route of
                        Routing.ItemList listData ->
                            let
                                oldFilter =
                                    listData.filter
                            in
                                { oldFilter | secondary = OnlySource sourceId }

                        _ ->
                            defaultFilter

                newModel =
                    changeFilter newFilter model
            in
                update (ShowItemList newFilter) newModel

        SelectPrimaryFilter filter ->
            let
                newFilter =
                    case model.route of
                        Routing.ItemList listData ->
                            let
                                oldFilter =
                                    listData.filter
                            in
                                { oldFilter | primary = filter }

                        _ ->
                            defaultFilter

                newModel =
                    changeFilter newFilter model
            in
                update (ShowItemList newFilter) newModel

        AuthForm msg ->
            let
                ( newModel, cmd ) =
                    AuthForm.update msg model.authForm
            in
                ( { model | authForm = newModel }, Cmd.map AuthForm cmd )

        SourceList msg ->
            let
                ( newModel, cmd ) =
                    SourceList.update msg model.sourceList
            in
                ( { model | sourceList = newModel }, Cmd.map SourceList cmd )

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
            ( { model
                | lang = lang
                , authForm = AuthForm.changeLang lang model.authForm
                , sourceList = SourceList.changeLang lang model.sourceList
              }
            , Cmd.none
            )

        OnLocationChange location ->
            let
                newRoute =
                    Routing.parseLocation location

                newTitle =
                    titleFor model newRoute

                newModel =
                    { model | title = newTitle, route = newRoute, error = Nothing }

                updateTitle =
                    if newTitle == model.title then
                        []
                    else
                        [setTitle newTitle]

                cmds =
                    Cmd.batch (updateTitle ++ extraCmds (Just model) newModel)
            in
                ( newModel, cmds )

        NoOp ->
            ( model, Cmd.none )


titleFor : Model -> Routing.Route -> String
titleFor model route =
    let
        tr =
            translate model.lang

        makeTitle msg =
            tr (Messages.Title (Just (tr msg)))
    in
        case route of
            Routing.ItemList { activeItem, filter } ->
                makeTitle Messages.ItemListTitle

            Routing.SourceList ->
                makeTitle Messages.SourceListTitle

            Routing.AuthError ->
                makeTitle Messages.AuthenticationTitle

            Routing.NotFoundRoute ->
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


navCollapsible : String -> Html Msg -> (key -> Msg) -> Maybe key -> List ( key, Html Msg ) -> Bool -> Msg -> Html Msg
navCollapsible widgetId label action activeKey items expanded toggleExpanded =
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
                    ( key, span [] [ text <| translate model.lang message, unreadBadge model (stat model.stats) ] )
            in
                List.map filterLabel [ ( AllItems, Messages.All, .total ), ( UnreadItems, Messages.Unread, .unread ), ( StarredItems, Messages.Starred, .starred ) ]

        activePrimaryFilter =
            case model.route of
                Routing.ItemList { filter } ->
                    Just filter.primary

                _ ->
                    Nothing
    in
        navCollapsible "nav-filters"
            (text <| translate model.lang Messages.Filters)
            SelectPrimaryFilter
            activePrimaryFilter
            filters
            model.filtersExpanded
            ToggleFiltersExpanded


navTags : Model -> Html Msg
navTags model =
    let
        activeTag =
            case model.route of
                Routing.ItemList { filter } ->
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

        tags =
            let
                tagLabel tag =
                    ( OnlyTag tag.tag, span [] [ text tag.tag, unreadBadge model tag.unread, span [ class "tag-color", Html.Attributes.style [ ( "background-color", tag.color ) ] ] [] ] )
            in
                ( AllTags, text <| translate model.lang Messages.AllTags ) :: List.map tagLabel model.tags
    in
        navCollapsible "nav-tags"
            (text <| translate model.lang Messages.Tags)
            toAction
            activeTag
            tags
            model.tagsExpanded
            ToggleTagsExpanded


navSources : Model -> Html Msg
navSources model =
    let
        activeSourceId =
            case model.route of
                Routing.ItemList { filter } ->
                    case filter.secondary of
                        OnlySource sourceId ->
                            Just sourceId

                        _ ->
                            Nothing

                _ ->
                    Nothing

        sources =
            let
                sourceLabel source =
                    ( source.id, span [] [ text source.title, unreadBadge model source.unread ] )
            in
                List.map sourceLabel model.sources
    in
        navCollapsible "nav-sources"
            (text <| translate model.lang Messages.Sources)
            SelectSource
            activeSourceId
            sources
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
            , button [ onClick RefreshTags ] [ text <| translate model.lang Messages.RefreshTags ]
            , button [ onClick RefreshSources ] [ text <| translate model.lang Messages.RefreshSources ]
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


tag : String -> Html Msg
tag t =
    span [ class "tag", onClick (SelectTag t) ] [ text t ]


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
                        [ span [ class "entry-tags" ] (List.map tag item.tags) ]

                    info =
                        List.intersperse (text " · ")
                            ([ source ] ++ author ++ [ a [ href item.url ] [ text item.datetime ] ])
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
    div [ class "almafoss", Html.Attributes.lang (Localization.Language.code model.lang) ]
        [ sidebar model
        , Html.main_ [] [ mainContent model ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    if Maybe.isJust model.error then
        Markdown.toHtml [] (Maybe.withDefault "" model.error)
    else if model.loading then
        text <| translate model.lang Messages.Loading
    else
        case model.route of
            Routing.AuthError ->
                AuthForm.authForm AuthForm model.authForm

            Routing.ItemList { activeItem } ->
                if List.isEmpty model.items then
                    text <| translate model.lang Messages.NoEntries
                else
                    let
                        entries =
                            List.map (entry model activeItem) model.items
                    in
                        div [ class "entries", role Role.Feed ] entries

            Routing.SourceList ->
                Html.map SourceList (SourceList.sourceList model.sourceList)
            Routing.NotFoundRoute ->
                text <| translate model.lang Messages.NotFound



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Kbd.subscriptions model.combos
        ]



-- HTTP


fetchItems : Model -> Cmd Msg
fetchItems model =
    let
        filter =
            case model.route of
                Routing.ItemList { filter } ->
                    filter

                _ ->
                    defaultFilter
    in
        Api.items
            model
            filter
            (\res ->
                case res of
                    Ok r ->
                        ItemsFetched r

                    Err e ->
                        FetchingFailed e
            )


fetchTags : Model -> Cmd Msg
fetchTags model =
    Api.tags
        model
        (\res ->
            case res of
                Ok r ->
                    TagsFetched r

                Err e ->
                    FetchingFailed e
        )


fetchStats : Model -> Cmd Msg
fetchStats model =
    Api.stats
        model
        (\res ->
            case res of
                Ok r ->
                    StatsFetched r

                Err e ->
                    FetchingFailed e
        )


fetchSources : Model -> Cmd Msg
fetchSources model =
    Api.sources
        model
        (\res ->
            case res of
                Ok r ->
                    SourcesFetched r

                Err e ->
                    FetchingFailed e
        )


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


updateItems : Int -> (Item -> Item) -> List DisplayItem -> List DisplayItem
updateItems itemId upd items =
    List.map
        (\item ->
            if item.item.id == itemId then
                { item | item = upd item.item }
            else
                item
        )
        items


changeFilter : Filter -> Model -> Model
changeFilter filter model =
    case model.route of
        Routing.ItemList listData ->
            let
                newRoute =
                    Routing.ItemList { listData | filter = filter }
            in
                { model | route = newRoute }

        _ ->
            model
