module Forms exposing (form, lineEdit, comboBox, password, button)

{-|
@docs form, lineEdit, comboBox, password, button
-}

import Html exposing (Html, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Kintail.InputWidget as InputWidget
import Utils exposing (onEnter)


type alias LineEditDef msg =
    { identifier : String
    , label : String
    , value : String
    , action : String -> msg
    }


type alias ComboBoxDef a msg =
    { identifier : String
    , label : String
    , printer : a -> String
    , values : List a
    , value : a
    , action : a -> msg
    }


type alias FormGroupDef msg =
    { identifier : String
    , label : String
    , control : Html msg
    }


type alias ButtonDef msg =
    { label : String
    , action : msg
    }


{-| -}
form : msg -> List (Html msg) -> Html msg
form saveAction content =
    Html.div [ class "form", onEnter saveAction ] content


formGroup : FormGroupDef msg -> Html msg
formGroup { identifier, label, control } =
    Html.div [ class "form-group" ]
        [ Html.label [ for identifier ] [ text <| label ]
        , Html.div [ class "form-control" ] [ control ]
        ]


{-| -}
lineEdit : LineEditDef msg -> Html msg
lineEdit { identifier, label, value, action } =
    formGroup
        { identifier = identifier
        , label = label
        , control = InputWidget.lineEdit [ id identifier ] value |> Html.map action
        }


{-| -}
password : LineEditDef msg -> Html msg
password { identifier, label, value, action } =
    formGroup
        { identifier = identifier
        , label = label
        , control = InputWidget.lineEdit [ id identifier, type_ "password" ] value |> Html.map action
        }


{-| -}
comboBox : ComboBoxDef a msg -> Html msg
comboBox { identifier, label, printer, values, value, action } =
    formGroup
        { identifier = identifier
        , label = label
        , control = InputWidget.comboBox [ id identifier ] printer values value |> Html.map action
        }


{-| -}
button : ButtonDef msg -> Html msg
button { label, action } =
    Html.div [ class "form-group" ]
        [ Html.button [ type_ "button", onClick action ] [ text label ]
        ]
