module Locale.Czech exposing (translator)

{-| Czech translation of the application.

@docs translator
-}

import Localization exposing (..)
import Localization.Pluralization as Pluralization
import Messages exposing (..)


pl : Int -> String -> String -> String -> Translation
pl n one few many =
    t (Pluralization.czech n one few many)


{-| -}
translator : Translator Message
translator str =
    case str of
        AppName ->
            t "álmafoss"

        Title page ->
            case page of
                Nothing ->
                    t "álmafoss"

                Just title ->
                    t (title ++ " – álmafoss")

        NotFoundTitle ->
            t "Stránka nenalezena"

        NotFound ->
            t "Požadovaná stránka nebyla nalezena."

        AuthenticationTitle ->
            t "Přihlášení"

        UserName ->
            t "Jméno"

        Password ->
            t "Heslo"

        SignIn ->
            t "Přihlásit se"

        SignOut ->
            t "Odhlásit se"

        AuthenticationRequired ->
            t "Pro vykonání této akce nemáte oprávnění. Přihlaste se prosím."

        IncorrectCredentials ->
            t "Špatné přihlašovací údaje."

        Filters ->
            t "Zobrazovat"

        All ->
            t "Vše"

        Unread ->
            t "Nepřečtené"

        Starred ->
            t "Označené"

        Tags ->
            t "Štítky"

        AllTags ->
            t "Všechny štítky"

        Sources ->
            t "Zdroje"

        NumberUnread n ->
            pl n
                (toString n ++ " nepřečtená")
                (toString n ++ " nepřečtené")
                (toString n ++ " nepřečtených")

        Refresh ->
            t "Obnovit"

        RefreshTags ->
            t "Obnovit štítky"

        RefreshSources ->
            t "Obnovit zdroje"

        ItemListTitle ->
            t "Seznam příspěvků"

        Loading ->
            t "Načítání obsahu…"

        NoEntries ->
            t "Nenalezeny žádné přípěvky."

        UnstarEntry ->
            t "Odznačit"

        StarEntry ->
            t "Označit"

        MarkEntryRead ->
            t "Označit jako přečtené"

        MarkEntryUnread ->
            t "Označit jako nepřečtené"

        ManageSources ->
            t "Spravovat zdroje"

        SourceListTitle ->
            t "Seznam zdrojů"

        AddSource ->
            t "Přidat zdroj"

        NoSources ->
            t "Nenalezeny žádné zdroje."

        EditSource ->
            t "Upravit"

        SaveSource ->
            t "Uložit"

        SourceSaved ->
            t "Změny uloženy"

        CancelSourceEditing ->
            t "Zrušit"

        DeleteSource ->
            t "Smazat"

        ReallyDeleteSource ->
            t "Opravdu si přejete smazat zdroj?"

        SourceTitle ->
            t "Název"

        SourceTags ->
            t "Štítky"

        SourceFilter ->
            t "Filtr"

        SourceSpout ->
            t "Typ"
