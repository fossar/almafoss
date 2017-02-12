module Utils exposing (maybeToCmd, onEnter, fromSqlDateTime)

{-| Utility functions for all the codebase.

## Other
@docs maybeToCmd, onEnter, fromSqlDateTime
-}

import Combine exposing ((<$>), (<*>), (*>), (<*), (>>=))
import Combine.Num
import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json
import Keyboard.Extra as Kbd
import Keyboard.Combo as Kbd
import Task
import Time.Date exposing (isValidDate)
import Time.DateTime exposing (DateTime, dateTime, isValidTime)


{-| Convert `Just action` to a command performing that action, `Nothing` results in an empty command.
-}
maybeToCmd : Maybe msg -> Cmd.Cmd msg
maybeToCmd action =
    case action of
        Nothing ->
            Cmd.none

        Just action ->
            Task.perform identity (Task.succeed action)


{-| Handle that enter was pressed.
-}
onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if Kbd.fromCode code == Kbd.enter then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


{-| Convert string containing fractionless ANSI SQL datetime to a DateTime type.
-}
fromSqlDateTime : String -> Result String DateTime
fromSqlDateTime input =
    let
        date =
            (,,)
                <$> Combine.Num.int
                <*> (Combine.string "-" *> intRange 1 12)
                <*> (Combine.string "-" *> intRange 1 31)

        time =
            (,,)
                <$> (Combine.string " " *> intRange 0 23)
                <*> (Combine.string ":" *> intRange 0 59)
                <*> (Combine.string ":" *> intRange 0 59)

        datetime =
            (,)
                <$> date
                <*> time
                <* Combine.end

        convert ( ( year, month, day ), ( hour, minute, second ) ) =
            if isValidDate year month day && isValidTime hour minute second 0 then
                { year = year, month = month, day = day, hour = hour, minute = minute, second = second, millisecond = 0 }
                    |> dateTime
                    |> Combine.succeed
            else
                Combine.fail "invalid date"
    in
        case Combine.parse (datetime >>= convert) input of
            Ok ( _, _, dt ) ->
                Ok dt

            Err ( _, { position }, es ) ->
                let
                    messages =
                        String.join " or " es
                in
                    Err ("Errors encountered at position " ++ toString position ++ ": " ++ messages)


paddedInt : Combine.Parser s Int
paddedInt =
    Combine.optional "" (Combine.string "0") *> Combine.Num.int


intRange : Int -> Int -> Combine.Parser s Int
intRange lo hi =
    let
        validate n =
            if n >= lo && n <= hi then
                Combine.succeed n
            else
                Combine.fail ("expected an integer in the range [" ++ toString lo ++ ", " ++ toString hi ++ "]")
    in
        paddedInt >>= validate
