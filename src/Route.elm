module Route exposing (..)

import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((<?>), Parser, map, oneOf, parse, s, top)
import Url.Parser.Query as Query


type Route
    = Home
    | SignIn (Maybe String)
    | Creatures
    | About
    | NotFound Url


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map About <| s "about"
        , map SignIn <| s "sign-in" <?> Query.string "redirect"
        ]


toRoute : Url -> Route
toRoute url =
    url
        |> parse route
        |> Maybe.withDefault (NotFound url)


toUrl : Route -> String
toUrl r =
    case r of
        Home ->
            "/"

        About ->
            "/about"

        Creatures ->
            "/creatures"

        SignIn redirect ->
            Builder.absolute [ "sign-in" ]
                (redirect
                    |> Maybe.map (Builder.string "redirect" >> List.singleton)
                    |> Maybe.withDefault []
                )

        NotFound url ->
            Url.toString url


matchAny : Route -> Route -> Maybe ()
matchAny any r =
    if any == r then
        Just ()

    else
        Nothing


matchHome : Route -> Maybe ()
matchHome =
    matchAny Home


matchAbout : Route -> Maybe ()
matchAbout r =
    case r of
        About ->
            Just ()

        _ ->
            Nothing


matchSignIn : Route -> Maybe (Maybe String)
matchSignIn r =
    case r of
        SignIn redirect ->
            Just redirect

        _ ->
            Nothing

matchCreatures : Route -> Maybe ()
matchCreatures =
    matchAny Creatures