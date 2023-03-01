module Pages.Creatures exposing (page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Shared exposing (Shared, update)
import Spa.Page
import View exposing (View)


type alias Model =
    { creatures : Dict Int Creature
    , templates : Dict String Creature
    , selectedCreature : String
    }


type Msg
    = SelectCreatureType String
    | NewCreature
    | UpdateCreature Int Creature
    | DeleteCreature Int
    | NoOp


page : Shared -> Spa.Page.Page () Shared.Msg (View Msg) Model Msg
page _ =
    Spa.Page.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : () -> ( Model, Effect Shared.Msg Msg )
init _ =
    { creatures = Dict.empty, templates = creatureTemplates, selectedCreature = "" } |> Effect.withNone


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg model =
    case msg of
        SelectCreatureType creatureType ->
            { model | selectedCreature = creatureType }
                |> Effect.withNone

        NewCreature ->
            let
                newCreature =
                    Dict.get model.selectedCreature model.templates
                        |> Maybe.withDefault
                            { name = ""
                            , str = initStat 0
                            , dex = initStat 0
                            , wil = initStat 0
                            , hp = initStat 0
                            , armor = Nothing
                            , attacks = ""
                            , criticalDamage = Nothing
                            , warbandScale = False
                            , notes = ""
                            }

                newId =
                    Dict.size model.creatures + 1
            in
            { model | creatures = Dict.insert newId newCreature model.creatures }
                |> Effect.withNone

        UpdateCreature id creature ->
            { model | creatures = Dict.insert id creature model.creatures }
                |> Effect.withNone

        DeleteCreature id ->
            { model | creatures = Dict.remove id model.creatures }
                |> Effect.withNone

        NoOp ->
            model |> Effect.withNone


view : Model -> View Msg
view model =
    { title = "Home"
    , body =
        div []
            [ h1 [ class "font-medieval text-3xl font-bold pb-8" ] [ text "Creatures" ]
            , div
                [ class
                    "flex flex-wrap gap-4 pb-8"
                ]
                [ div [ class "flex items-center" ]
                    [ select [ onInput SelectCreatureType, value model.selectedCreature, class "border border-black rounded-lg px-4 py-1" ]
                        (option [ value "" ] [ text "Choose..." ]
                            :: (Dict.toList model.templates
                                    |> List.map
                                        (\( name, _ ) ->
                                            option [ value name ] [ text name ]
                                        )
                               )
                        )
                    ]
                , button
                    [ class "font-medieval font-bold text-xl bg-gray-300 rounded-lg px-4 py-2 border border-black"
                    , onClick NewCreature
                    , disabled (model.selectedCreature == "")
                    ]
                    [ text "New Creature" ]
                ]
            , div [ class "flex flex-wrap gap-4" ]
                (Dict.toList model.creatures
                    |> List.map
                        (\( id, creature ) ->
                            div [ class "flex flex-col gap-4" ]
                                [ statBlock id creature
                                ]
                        )
                )
            ]
    }


type alias Creature =
    { name : String
    , str : Stat
    , dex : Stat
    , wil : Stat
    , hp : Stat
    , armor : Maybe Int
    , attacks : String
    , criticalDamage : Maybe String
    , warbandScale : Bool
    , notes : String
    }


type Stat
    = Stat ( Maybe Int, Maybe Int )


initStat : Int -> Stat
initStat value =
    Stat ( Just value, Just value )


max : Stat -> Maybe Int
max (Stat ( maxStat, _ )) =
    maxStat


current : Stat -> Maybe Int
current (Stat ( _, currentStat )) =
    currentStat


statBlock : Int -> Creature -> Html Msg
statBlock id creature =
    div []
        [ div [ class "flex flex-col gap-2 w-[300px]" ]
            [ nameRow creature { onDelete = DeleteCreature id, onUpdate = \name -> UpdateCreature id { creature | name = name } }
            , creatureDetails creature { onUpdate = UpdateCreature id }
            , div [ class "flex flex-row justify-between text-gray-500 text-xs h-[16px]" ]
                [ div [ class "w-[100px]" ] []
                , div [ class "flex-1 justify-center flex items-center" ] [ text "Max" ]
                , div [ class "flex-1 justify-center flex items-center" ] [ text "Current" ]
                ]
            , statRow "STR"
                creature.str
                { onUpdate =
                    \stat -> UpdateCreature id { creature | str = stat }
                }
            , statRow "DEX"
                creature.dex
                { onUpdate =
                    \stat -> UpdateCreature id { creature | dex = stat }
                }
            , statRow "WIL"
                creature.wil
                { onUpdate =
                    \stat -> UpdateCreature id { creature | wil = stat }
                }
            ]
        , div [ class "pt-6 w-[300px]" ]
            [ statRow "HP"
                creature.hp
                { onUpdate =
                    \stat -> UpdateCreature id { creature | hp = stat }
                }
            ]
        ]


creatureDetails : Creature -> { onUpdate : Creature -> Msg } -> Html Msg
creatureDetails creature { onUpdate } =
    div [ class "bg-gray-300 rounded-lg text-sm px-2 mx-2" ]
        [ viewIf (\_ -> div [ class "font-bold" ] [ text "Warband Scale" ]) creature.warbandScale
        , viewJust (\armor -> div [] [ text ("Armour " ++ String.fromInt armor) ]) creature.armor
        , div [] [ text ("Attacks: " ++ creature.attacks) ]
        , viewJust (\criticalDamage -> div [] [ text ("Critical Damage: " ++ criticalDamage) ]) creature.criticalDamage
        , text creature.notes
        ]


statRow : String -> Stat -> { onUpdate : Stat -> Msg } -> Html Msg
statRow label stat { onUpdate } =
    div [ class "font-medieval flex flex-row justify-between h-[30px] rounded-lg overflow-hidden font-bold text-xl border border-black" ]
        [ div [ class "bg-gray-300 flex items-center w-[100px] pl-6" ] [ text label ]
        , div [ class "flex-1 justify-center flex items-center border-r border-gray-200 m-2" ]
            [ input
                [ type_ "number"
                , class "w-[100%]"
                , value (max stat |> Maybe.map String.fromInt |> Maybe.withDefault "")
                , onInput
                    (\newStat -> onUpdate (Stat ( String.toInt newStat, current stat )))
                ]
                []
            ]
        , div [ class "flex-1 justify-center flex items-center m-2" ]
            [ input
                [ type_ "number"
                , class "w-[100%]"
                , value (current stat |> Maybe.map String.fromInt |> Maybe.withDefault "")
                , onInput
                    (\newStat -> onUpdate (Stat ( max stat, String.toInt newStat )))
                ]
                []
            ]
        ]


nameRow : Creature -> { onDelete : Msg, onUpdate : String -> Msg } -> Html Msg
nameRow { name } { onDelete, onUpdate } =
    div [ class "flex gap-2" ]
        [ div [ class "font-medieval flex flex-grow flex-row justify-between h-[30px] rounded-lg overflow-hidden font-bold text-xl border border-black" ]
            [ div [ class "bg-gray-300 flex items-center w-[100px] pl-6" ] [ text "Name" ]
            , div [ class "font-gochi flex-1 flex items-center m-2" ] [ input [ value name, onInput onUpdate, class "w-[100%]" ] [] ]
            ]

        -- button to delete this creature
        , button [ class "flex flex-shrink items-center justify-center w-[30px] h-[30px] bg-gray-300 hover:bg-gray-200 rounded-lg border border-black", onClick onDelete ] [ text "X" ]
        ]


viewJust : (a -> Html Msg) -> Maybe a -> Html Msg
viewJust content maybe =
    case maybe of
        Just a ->
            content a

        Nothing ->
            text ""


viewIf : (() -> Html Msg) -> Bool -> Html Msg
viewIf content condition =
    if condition then
        content ()

    else
        text ""



-- Built-in creatures


creatureTemplates : Dict String Creature
creatureTemplates =
    -- Cat, Centipede, Crow, Faerie, Frog, Ghost, Mouse, Owl, Rat, Snake, Spider
    Dict.fromList
        [ ( "Cat"
          , { name = "Cat"
            , str = initStat 15
            , dex = initStat 15
            , wil = initStat 10
            , hp = initStat 15
            , armor = Just 1
            , attacks = "d6 swipe, d8 bite"
            , criticalDamage = Nothing
            , warbandScale = True
            , notes = ""
            }
          )
        , ( "Centipede"
          , { name = "Centipede"
            , str = initStat 10
            , dex = initStat 12
            , wil = initStat 8
            , hp = initStat 8
            , armor = Just 1
            , attacks = "d6 venomous bite (damages DEX instead of STR)"
            , criticalDamage = Just "Venom takes effect, d12 damage to STR"
            , warbandScale = False
            , notes = ""
            }
          )
        , ( "Crow"
          , { name = "Crow"
            , str = initStat 12
            , dex = initStat 15
            , wil = initStat 15
            , hp = initStat 12
            , armor = Just 1
            , attacks = "d8 peck"
            , criticalDamage = Nothing
            , warbandScale = False
            , notes = "Flies 3x normal speed, knows two songs"
            }
          )
        , ( "Faerie"
          , { name = "Faerie"
            , str = initStat 10
            , dex = initStat 15
            , wil = initStat 15
            , hp = initStat 6
            , armor = Nothing
            , attacks = "d8 silver rapier"
            , criticalDamage = Nothing
            , warbandScale = False
            , notes = "Knows one spell"
            }
          )
        , ( "Frog"
          , { name = "Frog"
            , str = initStat 12
            , dex = initStat 15
            , wil = initStat 8
            , hp = initStat 6
            , armor = Just 1
            , attacks = "d10 spear or d6 tongue"
            , criticalDamage = Just "Leap out of reach"
            , warbandScale = False
            , notes = "Always goes first unless surprised, leaps 2x normal speed"
            }
          )
        , ( "Ghost"
          , { name = "Ghost"
            , str = initStat 5
            , dex = initStat 10
            , wil = initStat 10
            , hp = initStat 9
            , armor = Nothing
            , attacks = "ghostly power, d8 chilling touch (damages WIL)"
            , criticalDamage = Just "Possess the creature"
            , warbandScale = False
            , notes = "Only harmed by silver or magic weapons"
            }
          )
        , ( "Mouse"
          , { name = "Mouse"
            , str = initStat 9
            , dex = initStat 9
            , wil = initStat 9
            , hp = initStat 3
            , armor = Nothing
            , attacks = "d6 sword or d6 bow"
            , criticalDamage = Nothing
            , warbandScale = False
            , notes = ""
            }
          )
        , ( "Owl"
          , { name = "Owl"
            , str = initStat 15
            , dex = initStat 15
            , wil = initStat 15
            , hp = initStat 15
            , armor = Just 1
            , attacks = "d10 bite"
            , criticalDamage = Nothing
            , warbandScale = False
            , notes = "Flies 3x normal speed. Knows two spells"
            }
          )
        , ( "Rat"
          , { name = "Rat"
            , str = initStat 12
            , dex = initStat 8
            , wil = initStat 8
            , hp = initStat 3
            , armor = Nothing
            , attacks = "d6 cleaver"
            , criticalDamage = Nothing
            , warbandScale = False
            , notes = ""
            }
          )
        , ( "Snake"
          , { name = "Snake"
            , str = initStat 12
            , dex = initStat 10
            , wil = initStat 10
            , hp = initStat 12
            , armor = Just 2
            , attacks = "d8 bite"
            , criticalDamage = Just "Swallow whole, d4 STR damage per round until rescued or escape"
            , warbandScale = False
            , notes = ""
            }
          )
        , ( "Spider"
          , { name = "Spider"
            , str = initStat 8
            , dex = initStat 15
            , wil = initStat 10
            , hp = initStat 6
            , armor = Just 1
            , attacks = "d6 poison bite (damages DEX instead of STR)"
            , criticalDamage = Just "Carry away in web"
            , warbandScale = False
            , notes = ""
            }
          )
        ]
