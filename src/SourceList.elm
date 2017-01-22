module SourceList exposing (Model, Msg(..), init, changeLang, update, sourceList, fetchSourceData, fetchSpouts)

{-| This module takes care of user authentication.

## Types
@docs Model, Msg

## Model
@docs init, changeLang

## Update
@docs update, fetchSourceData, fetchSpouts

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
    { host : String
    , lang : Language
    , credentials : Maybe Credentials
    , data : WebData (List DisplaySourceData)
    , spouts : WebData (Dict String Spout)
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
    | SourceDataResponse (Result Http.Error (List SourceData))
    | SpoutsResponse (Result Http.Error (Dict String Spout))
    | SourceDataUpdateResponse SourceDataId (Result Http.Error Int)
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
            , data = NotAsked
            , spouts = NotAsked
            }
    in
        ( initialModel, Cmd.none )


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
        AddSource ->
            case model.data of
                RemoteData.Success data ->
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
                            { id = Temporary (nextSourceId data)
                            , source = newSourceData
                            , open = True
                            , modified = newSourceData
                            }

                        newData =
                            newDisplaySourceData :: data
                    in
                        ( { model | data = RemoteData.Success newData }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateSourceTitle sourceId title ->
            case model.data of
                RemoteData.Success data ->
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
                            List.updateIf (\source -> source.id == sourceId) upd data
                    in
                        ( { model | data = RemoteData.Success newData }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateSourceTags sourceId tags ->
            case model.data of
                RemoteData.Success data ->
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
                            List.updateIf (\source -> source.id == sourceId) upd data
                    in
                        ( { model | data = RemoteData.Success newData }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateSourceSpout sourceId spout ->
            case model.data of
                RemoteData.Success data ->
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
                            List.updateIf (\source -> source.id == sourceId) upd data
                    in
                        ( { model | data = RemoteData.Success newData }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Edit sourceId ->
            case model.data of
                RemoteData.Success data ->
                    let
                        upd source =
                            { source | open = True }

                        newData =
                            List.updateIf (\source -> source.id == sourceId) upd data
                    in
                        ( { model | data = RemoteData.Success newData }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Cancel sourceId ->
            case model.data of
                RemoteData.Success data ->
                    let
                        upd source =
                            { source | open = False, modified = source.source }

                        newData =
                            List.updateIf (\source -> source.id == sourceId) upd data
                    in
                        ( { model | data = RemoteData.Success newData }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Save sourceId ->
            case model.data of
                RemoteData.Success data ->
                    ( model, updateSourceData model sourceId )

                _ ->
                    ( model, Cmd.none )

        SourceDataUpdateResponse originalId respondedId ->
            case model.data of
                RemoteData.Success data ->
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
                                    List.updateIf (\source -> source.id == originalId) upd data
                            in
                                ( { model | data = RemoteData.Success newData }, Cmd.none )

                        Err err ->
                            ( { model | data = RemoteData.Failure err }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SourceDataResponse response ->
            case response of
                Ok data ->
                    let
                        sourceFmt src =
                            DisplaySourceData (Saved src.id) src False src
                    in
                        ( { model | data = RemoteData.Success (List.map sourceFmt data) }, Cmd.none )

                Err err ->
                    ( { model | data = RemoteData.Failure err }, Cmd.none )

        SpoutsResponse response ->
            case response of
                Ok spouts ->
                    ( { model | spouts = RemoteData.Success spouts }, Cmd.none )

                Err err ->
                    ( { model | spouts = RemoteData.Failure err }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


{-| Component listing sources and allowing their management.
-}
sourceList : Model -> Html Msg
sourceList model =
    case model.data of
        RemoteData.NotAsked ->
            Debug.crash "Trying to show source list and did not ask for data."

        RemoteData.Loading ->
            text <| translate model.lang Messages.Loading

        RemoteData.Success data ->
            if List.isEmpty data then
                text <| translate model.lang Messages.NoSources
            else
                let
                    sources =
                        List.map (sourceData model) data
                in
                    div []
                        [ button [ onClick AddSource ] [ text <| translate model.lang Messages.AddSource ]
                        , div [] sources
                        ]

        RemoteData.Failure err ->
            Markdown.toHtml [] (toString err)


sourceData : Model -> DisplaySourceData -> Html Msg
sourceData model data =
    let
        { source, open, modified } =
            data

        sourceId =
            data.id

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
            , div [ class "source-form source-content", onEnter (Save sourceId) ]
                [ Html.div [ class "form-group" ]
                    [ Html.label [ for (sourceFormEntryId "title") ] [ text <| translate model.lang Messages.SourceTitle ]
                    , Html.div [ class "form-control" ] [ InputWidget.lineEdit [ id (sourceFormEntryId "title") ] modified.title ] |> Html.map (UpdateSourceTitle sourceId)
                    ]
                , Html.div [ class "form-group" ]
                    [ Html.label [ for (sourceFormEntryId "tags") ] [ text <| translate model.lang Messages.SourceTags ]
                    , Html.div [ class "form-control" ] [ InputWidget.lineEdit [ id (sourceFormEntryId "tags") ] (String.join "," modified.tags) ] |> Html.map (UpdateSourceTags sourceId)
                    ]
                , Html.div [ class "form-group" ]
                    [ Html.label [ for (sourceFormEntryId "spout") ] [ text <| translate model.lang Messages.SourceSpout ]
                    , Html.div [ class "form-control" ] [ InputWidget.comboBox [ id (sourceFormEntryId "spout") ] getSpoutTitle (Dict.keys spouts) modified.spout |> Html.map (UpdateSourceSpout sourceId) ]
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


{-| -}
fetchSourceData : Model -> Cmd Msg
fetchSourceData model =
    Api.sourceData
        model
        SourceDataResponse


{-| -}
fetchSpouts : Model -> Cmd Msg
fetchSpouts model =
    Api.spouts
        model
        SpoutsResponse


updateSourceData : Model -> SourceDataId -> Cmd Msg
updateSourceData model originalId =
    model.data
        |> RemoteData.map
            (\data ->
                let
                    source =
                        case List.find (\source -> source.id == originalId) data of
                            Just source ->
                                source

                            Nothing ->
                                Debug.crash "Trying to save non-existent source."

                    call : Model -> SourceData -> (Result Http.Error Int -> Msg) -> Cmd Msg
                    call =
                        case originalId of
                            Temporary id ->
                                Api.addSource

                            Saved id ->
                                Api.updateSource
                in
                    call
                        model
                        source.modified
                        (SourceDataUpdateResponse originalId)
            )
        |> RemoteData.withDefault Cmd.none
