module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Css exposing (..)
import EverySet
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
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
        , view = view >> toUnstyled
        }



-- MODEL


type alias Model =
    { selected : EverySet.EverySet UUID.UUID
    , hands : Int
    , deck : Maybe Deck.Deck
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = EverySet.empty
      , hands = 4
      , deck = Nothing
      }
    , generateDeck
    )



-- UPDATE


type Msg
    = ToggleCardSelected UUID.UUID
    | GenerateDeck
    | DeckGenerated Deck.Deck
    | Draw Int
    | Discard
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

        Draw cards ->
            ( { model | deck = model.deck |> Maybe.map (Deck.draw cards) }, Cmd.none )

        Discard ->
            case model.deck of
                Nothing ->
                    ( model, Cmd.none )

                Just deck ->
                    ( { model
                        | deck = Just <| Deck.discard model.selected deck
                        , selected = EverySet.empty
                      }
                    , msgTask (Draw (EverySet.size model.selected))
                    )

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
            ( { model | deck = Just newDeck }, msgTask (Draw 8) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { deck, selected } =
    nav
        [ css
            [ displayFlex
            , alignItems center
            , flexDirection column
            ]
        ]
        [ h1 [ css [ textAlign center ] ] [ text "Elmatro" ]
        , handElement deck selected
        , button [ css [ marginTop <| px 12 ], onClick Discard ] [ text "Discard" ]
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
            div
                [ css
                    [ paddingTop <| px 100
                    , displayFlex
                    , flexDirection row
                    , justifyContent center
                    , height <| px 200
                    ]
                ]
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
cardElement msg selected card =
    div
        [ css
            [ position relative
            , bottom <|
                if selected then
                    px 50

                else
                    px 0
            ]
        ]
        [ button
            [ onClick msg
            , css
                [ fontSize <| rem 10
                , textAlign center
                ]
            ]
            [ text <|
                Deck.cardToUnicode card
            ]
        ]
