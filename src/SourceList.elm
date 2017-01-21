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
import Dict
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


type SourceDataId
    = Saved Int
    | Temporary Int


type alias DisplaySourceData =
    { id : SourceDataId
    , source : SourceData
    , open : Bool
    , modified : SourceData
    }


sourceIdToString : SourceDataId -> String
sourceIdToString sourceId =
    case sourceId of
        Saved id ->
            toString id

        Temporary id ->
            "unsaved-" ++ toString id


nextSourceId : List DisplaySourceData -> Int
nextSourceId data =
    let
        id source =
            case source.id of
                Saved id ->
                    id

                Temporary id ->
                    id

        mmax =
            List.maximum (List.map id data)

        max =
            Maybe.withDefault 0 mmax
    in
        max + 1


{-| Actions
-}
type Msg
    = AddSource
    | Save SourceDataId
    | Edit SourceDataId
    | Cancel SourceDataId
    | UpdateSourceTitle SourceDataId String
    | UpdateSourceTags SourceDataId String
    | UpdateSourceSpout SourceDataId String
    | SourceDataFetched (List SourceData)
    | SourceDataUpdated SourceDataId Int
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
    let
        _ = Debug.log "moo" action
        _ = Debug.log "moo" model
    in
    case action of
        AddSource ->
            let
                newSourceData =
                    { id = 0
                    , title = ""
                    , tags = []
                    , spout = ""
                    , params = Dict.empty
                    , error = Nothing
                    , lastentry = Nothing
                    , icon = Nothing
                    }

                newDisplaySourceData =
                    { id = Temporary (nextSourceId model.data)
                    , source = newSourceData
                    , open = True
                    , modified = newSourceData
                    }

                newData =
                    newDisplaySourceData :: model.data
            in
                ( { model | data = newData }, Cmd.none )

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
                    List.updateIf (\source -> source.id == sourceId) upd model.data
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
                    List.updateIf (\source -> source.id == sourceId) upd model.data
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
                    List.updateIf (\source -> source.id == sourceId) upd model.data
            in
                ( { model | data = newData }, Cmd.none )

        Edit sourceId ->
            let
                upd source =
                    { source | open = True }

                newData =
                    List.updateIf (\source -> source.id == sourceId) upd model.data
            in
                ( { model | data = newData }, Cmd.none )

        Cancel sourceId ->
            let
                upd source =
                    { source | open = False, modified = source.source }

                newData =
                    List.updateIf (\source -> source.id == sourceId) upd model.data
            in
                ( { model | data = newData }, Cmd.none )

        Save sourceId ->
            let
                upd source =
                    { source | open = False }

                newData =
                    List.updateIf (\source -> source.id == sourceId) upd model.data

                source =
                    Maybe.withDefault (Debug.crash "Trying to save non-existent source.") (List.find (\source -> source.id == sourceId) model.data)
            in
                ( { model | data = newData }, updateSourceData model sourceId source.source )

        SourceDataUpdated originalId newId ->
            let
                upd source =
                    { source | id = Saved newId }

                newData =
                    List.updateIf (\source -> source.id == originalId) upd model.data
            in
                ( { model | data = newData }, Cmd.none )

        SourceDataFetched data ->
            let
                sourceFmt src =
                    DisplaySourceData (Saved src.id) src False src
            in
                ( { model | data = List.map sourceFmt data }, Cmd.none )

        FetchingFailed err ->
            ( { model | error = Just err }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


{-| Component listing sources and allowing their management.
-}
sourceList : Model -> Html Msg
sourceList model =
    if List.isEmpty model.data then
        text <| translate model.lang Messages.NoSources
    else
        let
            sources =
                List.map (sourceData model) model.data
        in
            div []
                [ button [ onClick AddSource ] [ text <| translate model.lang Messages.AddSource ]
                , div [] sources
                ]


sourceData : Model -> DisplaySourceData -> Html Msg
sourceData model data =
    let
        { source, open, modified } =
            data

        sourceId =
            data.id
    in
        article [ id ("source-" ++ sourceIdToString sourceId), classList [ ( "source", True ), ( "open", open ) ], ariaExpanded (Just open) ]
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
                        [ button [ onClick (Edit sourceId) ]
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
                    [ Html.label [ for ("source-form-title" ++ toString sourceId) ] [ text <| translate model.lang Messages.SourceTitle ]
                    , Html.div [ class "form-control" ] [ input [ id ("source-form-title" ++ toString sourceId), type_ "text", onInput (UpdateSourceTitle sourceId), value modified.title, onEnter (Save sourceId) ] [] ]
                    ]
                , Html.div [ class "form-group" ]
                    [ Html.label [ for ("source-form-tags" ++ toString sourceId) ] [ text <| translate model.lang Messages.SourceTags ]
                    , Html.div [ class "form-control" ] [ input [ id ("source-form-tags" ++ toString sourceId), type_ "text", onInput (UpdateSourceTags sourceId), value (String.join "," modified.tags), onEnter (Save sourceId) ] [] ]
                    ]
                , Html.div [ class "form-group" ]
                    [ Html.label [ for ("source-form-spout" ++ toString sourceId) ] [ text <| translate model.lang Messages.SourceSpout ]
                    , Html.div [ class "form-control" ] [ input [ id ("source-form-spout" ++ toString sourceId), type_ "text", onInput (UpdateSourceSpout sourceId), value modified.spout, onEnter (Save sourceId) ] [] ]
                    ]
                ]
            , sourceDataPanel model sourceId
            ]


sourceDataPanel : Model -> SourceDataId -> Html Msg
sourceDataPanel model sourceId =
    let
        saveButton =
            [ button [ onClick (Save sourceId) ]
                [ text <| translate model.lang Messages.SaveSource ]
            ]

        cancelButton =
            [ button [ onClick (Cancel sourceId) ]
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


updateSourceData : Model -> SourceDataId -> SourceData -> Cmd Msg
updateSourceData model originalId data =
    let
        call =
            case originalId of
                Temporary id ->
                    Api.addSource

                Saved id ->
                    Api.updateSource
    in
        call
            model
            data
            (\res ->
                case res of
                    Ok newId ->
                        (SourceDataUpdated originalId newId)

                    Err e ->
                        (FetchingFailed e)
            )
