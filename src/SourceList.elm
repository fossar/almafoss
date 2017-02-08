module SourceList exposing (Model, Msg(..), DisplaySourceData, init, update, sourceList, fetchSourceData, fetchSpouts, updateSourceData)

{-| This module takes care of user authentication.

## Types
@docs Model, Msg, DisplaySourceData

## Model
@docs init

## Update
@docs update, fetchSourceData, fetchSpouts, updateSourceData

## View
@docs sourceList
-}

import Api
import Dict exposing (Dict)
import Html exposing (Html, a, article, button, div, h1, h2, header, img, input, li, nav, span, text, ul)
import Html.Accessibility exposing (ariaExpanded)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Kintail.InputWidget as InputWidget
import List.Extra as List
import Locale exposing (translate)
import Localization.Language exposing (Language, nativeName)
import Markdown
import Messages
import RemoteData exposing (RemoteData(..), WebData)
import Types exposing (..)
import Time.DateTime exposing (toISO8601)
import Utils exposing (onEnter)


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
    | Cancel SourceDataId
    | UpdateSourceTitle SourceDataId String
    | UpdateSourceTags SourceDataId String
    | UpdateSourceSpout SourceDataId String
    | SourceDataResponse (Result Http.Error (List SourceData))
    | SpoutsResponse (Result Http.Error (Dict String Spout))
    | SourceDataUpdateResponse SourceDataId (Result Http.Error Int)
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
update : Msg -> Model -> Model
update action model =
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
                            }

                        newData =
                            newDisplaySourceData :: sources
                    in
                        { model | sources = RemoteData.Success newData }

                _ ->
                    model

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
                        { model | sources = RemoteData.Success newData }

                _ ->
                    model

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
                        { model | sources = RemoteData.Success newData }

                _ ->
                    model

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
                        { model | sources = RemoteData.Success newData }

                _ ->
                    model

        Edit sourceId ->
            case model.sources of
                RemoteData.Success sources ->
                    let
                        upd source =
                            { source | open = True }

                        newData =
                            List.updateIf (\source -> source.id == sourceId) upd sources
                    in
                        { model | sources = RemoteData.Success newData }

                _ ->
                    model

        Save _ ->
            Debug.crash "Handled upstream"

        Cancel sourceId ->
            case model.sources of
                RemoteData.Success sources ->
                    let
                        upd source =
                            { source | open = False, modified = source.source }

                        newData =
                            List.updateIf (\source -> source.id == sourceId) upd sources
                    in
                        { model | sources = RemoteData.Success newData }

                _ ->
                    model

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
                                { model | sources = RemoteData.Success newData }

                        Err err ->
                            { model | sources = RemoteData.Failure err }

                _ ->
                    model

        SourceDataResponse response ->
            case response of
                Ok sources ->
                    let
                        sourceFmt src =
                            DisplaySourceData (Saved src.id) src False src
                    in
                        { model | sources = RemoteData.Success (List.map sourceFmt sources) }

                Err err ->
                    { model | sources = RemoteData.Failure err }

        SpoutsResponse response ->
            case response of
                Ok spouts ->
                    { model | spouts = RemoteData.Success spouts }

                Err err ->
                    { model | spouts = RemoteData.Failure err }

        NoOp ->
            model


{-| Component listing sources and allowing their management.
-}
sourceList : String -> Model -> Language -> Html Msg
sourceList host model lang =
    case model.sources of
        RemoteData.NotAsked ->
            Debug.crash "Trying to show source list and did not ask for data."

        RemoteData.Loading ->
            text <| translate lang Messages.Loading

        RemoteData.Success sources ->
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
        { source, open, modified } =
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
                        [ button [{- onClick (ToggleItemStarred source) -}]
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
            , div [ class "source-form source-content", onEnter (Save sourceId) ]
                [ Html.div [ class "form-group" ]
                    [ Html.label [ for (sourceFormEntryId "title") ] [ text <| translate lang Messages.SourceTitle ]
                    , Html.div [ class "form-control" ] [ InputWidget.lineEdit [ id (sourceFormEntryId "title") ] modified.title ] |> Html.map (UpdateSourceTitle sourceId)
                    ]
                , Html.div [ class "form-group" ]
                    [ Html.label [ for (sourceFormEntryId "tags") ] [ text <| translate lang Messages.SourceTags ]
                    , Html.div [ class "form-control" ] [ InputWidget.lineEdit [ id (sourceFormEntryId "tags") ] (String.join "," modified.tags) ] |> Html.map (UpdateSourceTags sourceId)
                    ]
                , Html.div [ class "form-group" ]
                    [ Html.label [ for (sourceFormEntryId "spout") ] [ text <| translate lang Messages.SourceSpout ]
                    , Html.div [ class "form-control" ] [ InputWidget.comboBox [ id (sourceFormEntryId "spout") ] getSpoutTitle (Dict.keys spouts) modified.spout |> Html.map (UpdateSourceSpout sourceId) ]
                    ]
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
