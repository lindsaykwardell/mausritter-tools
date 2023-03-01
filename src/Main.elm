module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.About as About
import Pages.Creatures as Creatures
import Route
import Shared exposing (Shared)
import Spa
import View exposing (View)


mappers : ( (a -> b) -> View a -> View b, (c -> d) -> View c -> View d )
mappers =
    ( View.map, View.map )


toDocument :
    Shared
    -> View (Spa.Msg Shared.Msg pageMsg)
    -> Document (Spa.Msg Shared.Msg pageMsg)
toDocument _ view =
    { title = view.title
    , body =
        [ div
            []
            [ main_ [ class "bg-white m-8 p-3" ] [ view.body ]
            ]
        ]
    }


main =
    Spa.init
        { defaultView = View.defaultView
        , extractIdentity = Shared.identity
        }
        |> Spa.addPublicPage mappers Route.matchHome Creatures.page
        |> Spa.addPublicPage mappers Route.matchAbout About.page
        |> Spa.application View.map
            { init = Shared.init
            , subscriptions = Shared.subscriptions
            , update = Shared.update
            , toRoute = Route.toRoute
            , toDocument = toDocument
            , protectPage = Route.toUrl >> Just >> Route.SignIn >> Route.toUrl
            }
        |> Browser.application
