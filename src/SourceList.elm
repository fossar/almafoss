module SourceList exposing (Model, Msg(..), DisplaySourceData, init, update, sourceList, fetchSourceData, fetchSpouts)

{-| This module takes care of user authentication.

## Types
@docs Model, Msg, DisplaySourceData

## Model
@docs init

## Update
@docs update, fetchSourceData, fetchSpouts

## View
@docs sourceList
-}

import Api
import Dict exposing (Dict)
import Forms
import Html exposing (Html, a, article, button, div, h1, h2, header, img, input, li, nav, span, text, ul)
import Html.Accessibility exposing (ariaExpanded)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import List.Extra as List
import Locale exposing (translate)
import Localization.Language exposing (Language, nativeName)
import Markdown
import Maybe.Extra as Maybe
import Messages
import RemoteData exposing (RemoteData(..), WebData)
import Rocket exposing ((=>))
import Task
import Time.DateTime exposing (toISO8601)
import Types exposing (..)


{-| Model of the authentication module.
-}
type alias Model =
    { sources : WebData (List DisplaySourceData)
    , spouts : WebData (Dict String Spout)
    }


type SourceDataId
    = Saved Int
    | Temporary Int


{-| -}
type alias DisplaySourceData =
    { id : SourceDataId
    , source : SourceData
    , open : Bool
    , modified : SourceData
    , deleting : Bool
    }


sourceIdToString : SourceDataId -> String
sourceIdToString sourceId =
    case sourceId of
        Saved id ->
            toString id

        Temporary id ->
            "unsaved-" ++ toString id


nextSourceId : List DisplaySourceData -> Int
nextSourceId sources =
    let
        id source =
            case source.id of
                Saved id ->
                    id

                Temporary id ->
                    id

        mmax =
            List.maximum (List.map id sources)

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
    | ToggleDeletion SourceDataId
    | Delete SourceDataId
    | Cancel SourceDataId
    | UpdateSourceTitle SourceDataId String
    | UpdateSourceTags SourceDataId String
    | UpdateSourceSpout SourceDataId String
    | UpdateSourceParam SourceDataId String String
    | SourceDataResponse (Result Http.Error (List SourceData))
    | SpoutsResponse (Result Http.Error (Dict String Spout))
    | SourceDataUpdateResponse SourceDataId (Result Http.Error Int)
    | SourceDataDeleteResponse SourceDataId (Result Http.Error Bool)
    | NoOp


{-| Initialise model
-}
init : Model
init =
    let
        initialModel =
            { sources = NotAsked
            , spouts = NotAsked
            }
    in
        initialModel


{-| Update module state based on an action.
-}
update : Maybe Credentials -> String -> Msg -> Model -> ( Model, Cmd Msg )
update credentials host action model =
    case action of
        AddSource ->
            case model.sources of
                RemoteData.Success sources ->
                    let
                        newSourceData =
                            { id = 0
                            , title = ""
                            , tags = []
                            , spout = "spouts\\rss\\feed"
                            , params = Dict.empty
                            , error = Nothing
                            , lastentry = Nothing
                            , icon = Nothing
                            }

                        newDisplaySourceData =
                            { id = Temporary (nextSourceId sources)
                            , source = newSourceData
                            , open = True
                            , modified = newSourceData
                            , deleting = False
                            }

                        newData =
                            newDisplaySourceData :: sources
                    in
                        { model | sources = RemoteData.Success newData } => Cmd.none

                _ ->
                    model => Cmd.none

        UpdateSourceTitle sourceId title ->
            case model.sources of
                RemoteData.Success sources ->
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
                            List.updateIf (\source -> source.id == sourceId) upd sources
                    in
                        { model | sources = RemoteData.Success newData } => Cmd.none

                _ ->
                    model => Cmd.none

        UpdateSourceTags sourceId tags ->
            case model.sources of
                RemoteData.Success sources ->
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
                            List.updateIf (\source -> source.id == sourceId) upd sources
                    in
                        { model | sources = RemoteData.Success newData } => Cmd.none

                _ ->
                    model => Cmd.none

        UpdateSourceSpout sourceId spout ->
            case model.sources of
                RemoteData.Success sources ->
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
                            List.updateIf (\source -> source.id == sourceId) upd sources
                    in
                        { model | sources = RemoteData.Success newData } => Cmd.none

                _ ->
                    model => Cmd.none

        UpdateSourceParam sourceId name value ->
            (mapSources << RemoteData.map)
                (List.updateIf
                    (\source -> source.id == sourceId)
                    ((mapModified << mapParams) (Dict.insert name (Just value)))
                )
                model
                => Cmd.none

        Edit sourceId ->
            case model.sources of
                RemoteData.Success sources ->
                    let
                        upd source =
                            { source | open = True }

                        newData =
                            List.updateIf (\source -> source.id == sourceId) upd sources
                    in
                        { model | sources = RemoteData.Success newData } => Cmd.none

                _ ->
                    model => Cmd.none

        ToggleDeletion sourceId ->
            (mapSources << RemoteData.map)
                (List.updateIf
                    (\source -> source.id == sourceId)
                    (\data -> { data | deleting = not data.deleting })
                )
                model
                => Cmd.none

        Save sourceId ->
            case model.sources of
                RemoteData.Success sources ->
                    let
                        cmd =
                            updateSourceData credentials host model sourceId
                    in
                        ( model, cmd )

                _ ->
                    ( model, Cmd.none )

        Delete sourceId ->
            case model.sources of
                RemoteData.Success sources ->
                    let
                        cmd =
                            deleteSourceData credentials host model sourceId
                    in
                        ( model, cmd )

                _ ->
                    ( model, Cmd.none )

        Cancel sourceId ->
            case model.sources of
                RemoteData.Success sources ->
                    let
                        upd source =
                            { source | open = False, modified = source.source }

                        newData =
                            List.updateIf (\source -> source.id == sourceId) upd sources
                    in
                        { model | sources = RemoteData.Success newData } => Cmd.none

                _ ->
                    model => Cmd.none

        SourceDataUpdateResponse originalId respondedId ->
            case model.sources of
                RemoteData.Success sources ->
                    case respondedId of
                        Ok newId ->
                            let
                                upd source =
                                    let
                                        oldData =
                                            source.modified

                                        newData =
                                            { oldData | id = newId }
                                    in
                                        { source | id = Saved newId, source = newData, modified = newData }

                                newData =
                                    List.updateIf (\source -> source.id == originalId) upd sources
                            in
                                { model | sources = RemoteData.Success newData } => Cmd.none

                        Err err ->
                            { model | sources = RemoteData.Failure err } => Cmd.none

                _ ->
                    model => Cmd.none

        SourceDataDeleteResponse originalId response ->
            case model.sources of
                RemoteData.Success sources ->
                    case response of
                        Ok _ ->
                            let
                                newData =
                                    List.filter (\source -> source.id /= originalId) sources
                            in
                                { model | sources = RemoteData.Success newData } => Cmd.none

                        Err err ->
                            { model | sources = RemoteData.Failure err } => Cmd.none

                _ ->
                    model => Cmd.none

        SourceDataResponse response ->
            case response of
                Ok sources ->
                    let
                        sourceFmt src =
                            DisplaySourceData (Saved src.id) src False src False
                    in
                        { model | sources = RemoteData.Success (List.map sourceFmt sources) } => Cmd.none

                Err err ->
                    { model | sources = RemoteData.Failure err } => Cmd.none

        SpoutsResponse response ->
            case response of
                Ok spouts ->
                    { model | spouts = RemoteData.Success spouts } => Cmd.none

                Err err ->
                    { model | spouts = RemoteData.Failure err } => Cmd.none

        NoOp ->
            model => Cmd.none


{-| Component listing sources and allowing their management.
-}
sourceList : String -> Model -> Language -> Html Msg
sourceList host model lang =
    case RemoteData.append model.sources model.spouts of
        RemoteData.NotAsked ->
            Debug.crash "Trying to show source list and did not ask for data."

        RemoteData.Loading ->
            text <| translate lang Messages.Loading

        RemoteData.Success ( sources, spouts ) ->
            if List.isEmpty sources then
                text <| translate lang Messages.NoSources
            else
                let
                    sourceDataList =
                        List.map (sourceData host model lang) sources
                in
                    div []
                        [ button [ onClick AddSource ] [ text <| translate lang Messages.AddSource ]
                        , div [] sourceDataList
                        ]

        RemoteData.Failure err ->
            Markdown.toHtml [] (toString err)


sourceData : String -> Model -> Language -> DisplaySourceData -> Html Msg
sourceData host model lang sources =
    let
        { source, open, modified, deleting } =
            sources

        sourceId =
            sources.id

        spouts =
            RemoteData.withDefault Dict.empty model.spouts

        getSpoutTitle spoutId =
            case Dict.get spoutId spouts of
                Just spout ->
                    spout.name

                Nothing ->
                    Debug.crash "Trying to get title of non-existent spout."

        getSpoutParams spoutId =
            case Dict.get spoutId spouts of
                Just spout ->
                    spout.params

                Nothing ->
                    Debug.crash "Trying to get params of non-existent spout."

        sourceFormEntryId entry =
            "source-" ++ sourceIdToString sourceId ++ "-form-" ++ entry
    in
        article [ id ("source-" ++ sourceIdToString sourceId), classList [ ( "source", True ), ( "open", open ) ], ariaExpanded (Just open) ]
            [ header [ class "source-header" ]
                (let
                    icon =
                        case source.icon of
                            Just icon ->
                                [ img [ src (host ++ "/data/favicons/" ++ icon), alt "", width 16, height 16, class "source-icon" ] [] ]

                            Nothing ->
                                []

                    title =
                        [ h1 [] [ text source.title ] ]

                    editButton =
                        [ button [ onClick (Edit sourceId) ]
                            [ text <| translate lang Messages.EditSource ]
                        ]

                    deleteButton =
                        if deleting then
                            [ text <| translate lang Messages.ReallyDeleteSource
                            , button [ onClick (Delete sourceId) ] [ text <| translate lang Messages.YesDeleteSource ]
                            , button [ onClick (ToggleDeletion sourceId) ] [ text <| translate lang Messages.NoDeleteSource ]
                            ]
                        else
                            [ button [ onClick (ToggleDeletion sourceId) ]
                                [ text <| translate lang Messages.DeleteSource ]
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
            , div [ class "source-content" ]
                [ Forms.form (Save sourceId)
                    (let
                        mainFields =
                            [ Forms.lineEdit
                                { identifier = (sourceFormEntryId "title")
                                , label = translate lang Messages.SourceTitle
                                , value = modified.title
                                , action = UpdateSourceTitle sourceId
                                }
                            , Forms.lineEdit
                                { identifier = sourceFormEntryId "tags"
                                , label = translate lang Messages.SourceTags
                                , value = String.join "," modified.tags
                                , action = UpdateSourceTags sourceId
                                }
                            , Forms.comboBox
                                { identifier = sourceFormEntryId "spout"
                                , label = translate lang Messages.SourceSpout
                                , printer = getSpoutTitle
                                , values = Dict.keys spouts
                                , value = modified.spout
                                , action = UpdateSourceSpout sourceId
                                }
                            ]

                        paramFields =
                            Dict.toList (getSpoutParams modified.spout)
                                |> List.map
                                    (\( name, param ) ->
                                        let
                                            paramValue =
                                                Maybe.withDefault param.default (Maybe.join (Dict.get name modified.params))

                                            simpleData =
                                                { identifier = sourceFormEntryId name
                                                , label = param.title
                                                , value = paramValue
                                                , action = UpdateSourceParam sourceId name
                                                }

                                            getComboItemLabel items id =
                                                case Dict.get id items of
                                                    Just value ->
                                                        value

                                                    Nothing ->
                                                        Debug.crash "Trying to get label of a non-existent item."
                                        in
                                            case param.class of
                                                SpoutParamCheckbox ->
                                                    Forms.lineEdit simpleData

                                                SpoutParamPassword ->
                                                    Forms.password simpleData

                                                SpoutParamSelect ->
                                                    let
                                                        values =
                                                            Maybe.withDefault Dict.empty param.values
                                                    in
                                                        Forms.comboBox
                                                            { identifier = sourceFormEntryId name
                                                            , label = param.title
                                                            , printer = getComboItemLabel values
                                                            , value = paramValue
                                                            , values = Dict.keys values
                                                            , action = UpdateSourceParam sourceId name
                                                            }

                                                SpoutParamText ->
                                                    Forms.lineEdit simpleData

                                                SpoutParamUrl ->
                                                    Forms.url simpleData
                                    )
                     in
                        mainFields ++ paramFields
                    )
                ]
            , sourceDataPanel model lang sourceId
            ]


sourceDataPanel : Model -> Language -> SourceDataId -> Html Msg
sourceDataPanel model lang sourceId =
    let
        saveButton =
            [ button [ onClick (Save sourceId) ]
                [ text <| translate lang Messages.SaveSource ]
            ]

        cancelButton =
            [ button [ onClick (Cancel sourceId) ]
                [ text <| translate lang Messages.CancelSourceEditing ]
            ]
    in
        div [ class "source-panel" ]
            (saveButton ++ cancelButton)


{-| -}
fetchSourceData : Maybe Credentials -> String -> Cmd Msg
fetchSourceData credentials host =
    let
        requestModel =
            { credentials = credentials, host = host }
    in
        Api.sourceData
            requestModel
            SourceDataResponse


{-| -}
fetchSpouts : Maybe Credentials -> String -> Cmd Msg
fetchSpouts credentials host =
    let
        requestModel =
            { credentials = credentials, host = host }
    in
        Api.spouts
            requestModel
            SpoutsResponse


{-| -}
updateSourceData : Maybe Credentials -> String -> Model -> SourceDataId -> Cmd Msg
updateSourceData credentials host model originalId =
    let
        requestModel =
            { credentials = credentials, host = host }
    in
        model.sources
            |> RemoteData.map
                (\sources ->
                    let
                        source =
                            case List.find (\source -> source.id == originalId) sources of
                                Just source ->
                                    source

                                Nothing ->
                                    Debug.crash "Trying to save non-existent source."

                        call : { x | credentials : Maybe Credentials, host : String } -> SourceData -> (Result Http.Error Int -> Msg) -> Cmd Msg
                        call =
                            case originalId of
                                Temporary id ->
                                    Api.addSource

                                Saved id ->
                                    Api.updateSource
                    in
                        call
                            requestModel
                            source.modified
                            (SourceDataUpdateResponse originalId)
                )
            |> RemoteData.withDefault Cmd.none


{-| -}
deleteSourceData : Maybe Credentials -> String -> Model -> SourceDataId -> Cmd Msg
deleteSourceData credentials host model originalId =
    let
        requestModel =
            { credentials = credentials, host = host }
    in
        case originalId of
            Temporary id ->
                Task.perform identity (Task.succeed (SourceDataDeleteResponse originalId (Ok True)))

            Saved id ->
                model.sources
                    |> RemoteData.map
                        (\sources ->
                            let
                                source =
                                    case List.find (\source -> source.id == originalId) sources of
                                        Just source ->
                                            source

                                        Nothing ->
                                            Debug.crash "Trying to delete non-existent source."
                            in
                                Api.deleteSource
                                    requestModel
                                    id
                                    (SourceDataDeleteResponse originalId)
                        )
                    |> RemoteData.withDefault Cmd.none



-- SETTERS


mapModified : Mapper DisplaySourceData SourceData
mapModified f display =
    { display | modified = f display.modified }


mapParams : Mapper SourceData (Dict String (Maybe String))
mapParams f data =
    { data | params = f data.params }


mapSources : Mapper Model (WebData (List DisplaySourceData))
mapSources f model =
    { model | sources = f model.sources }
