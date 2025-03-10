module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import EverySet
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model.Deck as Deck
import Random
import Task
import UUID



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { selected : EverySet.EverySet UUID.UUID
    , deck : Maybe Deck.Deck
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = EverySet.empty
      , deck = Nothing
      }
    , generateDeck
    )



-- UPDATE


type Msg
    = ToggleCardSelected UUID.UUID
    | GenerateDeck
    | DeckGenerated Deck.Deck
    | Draw
    | Shuffle
    | Shuffled Deck.Deck


generateDeck : Cmd Msg
generateDeck =
    Random.generate DeckGenerated (Deck.mkDeck ())


msgTask : Msg -> Cmd Msg
msgTask msg =
    Task.succeed msg |> Task.perform identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCardSelected cardId ->
            let
                isSelected =
                    EverySet.member cardId model.selected

                modify =
                    if isSelected then
                        EverySet.remove

                    else
                        EverySet.insert
            in
            ( { model | selected = modify cardId model.selected }, Cmd.none )

        Draw ->
            ( { model | deck = model.deck |> Maybe.map (Deck.draw 5) }, Cmd.none )

        GenerateDeck ->
            ( model, generateDeck )

        DeckGenerated deck ->
            ( { model | deck = Just deck }, msgTask Shuffle )

        Shuffle ->
            case model.deck of
                Nothing ->
                    ( model, Cmd.none )

                Just deck ->
                    ( model, Random.generate Shuffled (Deck.shuffle deck) )

        Shuffled newDeck ->
            ( { model | deck = Just newDeck }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { deck, selected } =
    Html.div []
        [ Html.button [ onClick Draw ] [ text "Draw" ]
        , handElement deck selected
        ]


handElement : Maybe Deck.Deck -> EverySet.EverySet UUID.UUID -> Html Msg
handElement mDeck selected =
    let
        isSelected card =
            EverySet.member card.id selected
    in
    case mDeck of
        Nothing ->
            div [] []

        Just deck ->
            div []
                (deck.hand
                    |> List.map
                        (\card ->
                            cardElement
                                (ToggleCardSelected card.id)
                                (isSelected card)
                                card
                        )
                )


cardElement : Msg -> Bool -> Deck.Card -> Html Msg
cardElement msg selected { suit, rank } =
    label
        [ style "padding" "20px" ]
        [ input [ type_ "checkbox", checked selected, onClick msg ] []
        , text
            (String.join
                ""
                [ Deck.rankToString rank
                , " of "
                , Deck.suitToString suit
                ]
            )
        ]
