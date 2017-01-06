module SourceList exposing (Model, Msg(..), init, changeLang, update, sourceList)

{-| This module takes care of user authentication.

## Types
@docs Model, Msg

## Model
@docs init, changeLang

## Update
@docs update

## View
@docs sourceList
-}

import Api
import Html exposing (Html, a, article, button, div, h1, h2, header, img, input, li, nav, span, text, ul)
import Html.Accessibility exposing (ariaExpanded)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import List.Extra as List
import Locale exposing (translate)
import Localization.Language exposing (Language, nativeName)
import Messages
import Types exposing (..)
import Time.DateTime exposing (toISO8601)
import Utils exposing (onEnter)


{-| Model of the authentication module.
-}
type alias Model =
    { host : String
    , lang : Language
    , credentials : Maybe Credentials
    , data : List DisplaySourceData
    , error : Maybe Http.Error
    }


type alias DisplaySourceData =
    { source : SourceData
    , open : Bool
    , modified : SourceData
    }


{-| Actions
-}
type Msg
    = Authenticate
    | Edit Int
    | Cancel Int
    | UpdateSourceTitle Int String
    | UpdateSourceTags Int String
    | UpdateSourceSpout Int String
    | SourceDataFetched (List SourceData)
    | FetchingFailed Http.Error
    | NoOp


{-| Initialise model
-}
init : String -> Language -> Maybe Credentials -> ( Model, Cmd Msg )
init host lang credentials =
    let
        initialModel =
            { host = host
            , lang = lang
            , credentials = credentials
            , data = []
            , error = Nothing
            }
    in
        ( initialModel, fetchSourceData initialModel )


{-| Change language of the component
-}
changeLang : Language -> Model -> Model
changeLang lang model =
    { model | lang = lang }


{-| Update module state based on an action.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Authenticate ->
            ( model, Cmd.none )

        UpdateSourceTitle sourceId title ->
            let
                upd source =
                    let
                        oldModified =
                            source.modified

                        newModified =
                            { oldModified | title = title }
                    in
                        { source | modified = newModified }

                newData =
                    List.updateIf (\source -> source.source.id == sourceId) upd model.data
            in
                ( { model | data = newData }, Cmd.none )

        UpdateSourceTags sourceId tags ->
            let
                upd source =
                    let
                        oldModified =
                            source.modified

                        newModified =
                            { oldModified | tags = (String.split "," tags) }
                    in
                        { source | modified = newModified }

                newData =
                    List.updateIf (\source -> source.source.id == sourceId) upd model.data
            in
                ( { model | data = newData }, Cmd.none )

        UpdateSourceSpout sourceId spout ->
            let
                upd source =
                    let
                        oldModified =
                            source.modified

                        newModified =
                            { oldModified | spout = spout }
                    in
                        { source | modified = newModified }

                newData =
                    List.updateIf (\source -> source.source.id == sourceId) upd model.data
            in
                ( { model | data = newData }, Cmd.none )

        Edit sourceId ->
            let
                upd source =
                    { source | open = True }

                newData =
                    List.updateIf (\source -> source.source.id == sourceId) upd model.data
            in
                ( { model | data = newData }, Cmd.none )

        Cancel sourceId ->
            let
                upd source =
                    { source | open = False, modified = source.source }

                newData =
                    List.updateIf (\source -> source.source.id == sourceId) upd model.data
            in
                ( { model | data = newData }, Cmd.none )

        SourceDataFetched data ->
            let
                sourceFmt src =
                    DisplaySourceData src False src
            in
                ( { model | data = List.map sourceFmt data }, Cmd.none )

        FetchingFailed err ->
            ( { model | error = Just err }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


sourceList : Model -> Html Msg
sourceList model =
    if List.isEmpty model.data then
        text <| translate model.lang Messages.NoSources
    else
        let
            sources =
                List.map (sourceData model) model.data
        in
            div [ class "sources" ] sources


sourceData : Model -> DisplaySourceData -> Html Msg
sourceData model { source, open, modified } =
    article [ id ("source-" ++ toString source.id), classList [ ( "source", True ), ( "open", open ) ], ariaExpanded (Just open) ]
        [ header [ class "source-header" ]
            (let
                icon =
                    case source.icon of
                        Just icon ->
                            [ img [ src (model.host ++ "/data/favicons/" ++ icon), alt "", width 16, height 16, class "source-icon" ] [] ]

                        Nothing ->
                            []

                title =
                    [ h1 [] [ text source.title ] ]

                editButton =
                    [ button [ onClick (Edit source.id) ]
                        [ text <| translate model.lang Messages.EditSource ]
                    ]

                deleteButton =
                    [ button [{- onClick (ToggleItemStarred source) -}]
                        [ text <| translate model.lang Messages.DeleteSource ]
                    ]

                lastUpdated =
                    case source.lastentry of
                        Just datetime ->
                            [ text (toISO8601 datetime) ]

                        Nothing ->
                            []

                info =
                    List.intersperse (text " · ") (editButton ++ deleteButton ++ lastUpdated)
             in
                icon ++ title ++ info
            )
        , div [ class "source-form source-content" ]
            [ Html.div [ class "form-group" ]
                [ Html.label [ for ("source-form-title" ++ toString source.id) ] [ text <| translate model.lang Messages.SourceTitle ]
                , Html.div [ class "form-control" ] [ input [ id ("source-form-title" ++ toString source.id), type_ "text", onInput (UpdateSourceTitle source.id), value modified.title, onEnter (Authenticate) ] [] ]
                ]
            , Html.div [ class "form-group" ]
                [ Html.label [ for ("source-form-tags" ++ toString source.id) ] [ text <| translate model.lang Messages.SourceTags ]
                , Html.div [ class "form-control" ] [ input [ id ("source-form-tags" ++ toString source.id), type_ "text", onInput (UpdateSourceTags source.id), value (String.join "," modified.tags), onEnter (Authenticate) ] [] ]
                ]
            , Html.div [ class "form-group" ]
                [ Html.label [ for ("source-form-spout" ++ toString source.id) ] [ text <| translate model.lang Messages.SourceSpout ]
                , Html.div [ class "form-control" ] [ input [ id ("source-form-spout" ++ toString source.id), type_ "text", onInput (UpdateSourceSpout source.id), value modified.spout, onEnter (Authenticate) ] [] ]
                ]
            ]
        , sourceDataPanel model source
        ]


sourceDataPanel : Model -> SourceData -> Html Msg
sourceDataPanel model data =
    let
        saveButton =
            [ button [ onClick (NoOp) ]
                [ text <| translate model.lang Messages.SaveSource ]
            ]

        cancelButton =
            [ button [ onClick (Cancel data.id) ]
                [ text <| translate model.lang Messages.CancelSourceEditing ]
            ]
    in
        div [ class "source-panel" ]
            (saveButton ++ cancelButton)


fetchSourceData : Model -> Cmd Msg
fetchSourceData model =
    Api.sourceData
        model
        (\res ->
            case res of
                Ok r ->
                    (SourceDataFetched r)

                Err e ->
                    (FetchingFailed e)
        )
