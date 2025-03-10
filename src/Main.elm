module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Css exposing (..)
import Css.Global
import EverySet
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model.Card as Card
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
    , discards : Int
    , deck : Maybe Deck.Deck
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = EverySet.empty
      , hands = 4
      , discards = 3
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

                amountSelected =
                    EverySet.size model.selected

                modify =
                    case ( isSelected, amountSelected >= 5 ) of
                        ( True, _ ) ->
                            EverySet.remove

                        ( False, False ) ->
                            EverySet.insert

                        ( False, True ) ->
                            \_ -> identity
            in
            ( { model | selected = modify cardId model.selected }, Cmd.none )

        Draw cards ->
            ( { model | deck = model.deck |> Maybe.map (Deck.draw cards) }, Cmd.none )

        Discard ->
            case ( model.deck, model.discards ) of
                ( Nothing, _ ) ->
                    ( model, Cmd.none )

                ( _, 0 ) ->
                    ( model, Cmd.none )

                ( Just deck, discards ) ->
                    ( { model
                        | deck = Just <| Deck.discard model.selected deck
                        , selected = EverySet.empty
                        , discards = discards - 1
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
view { deck, selected, hands, discards } =
    nav
        [ css
            [ displayFlex
            , alignItems center
            , flexDirection column
            ]
        ]
        [ h1 [ css [ textAlign center ] ] [ text "Elmatro" ]
        , div [ css [ displayFlex, flexDirection column ] ]
            [ span [] [ text <| ("Hands: " ++ String.fromInt hands) ]
            , span [] [ text <| ("Discards: " ++ String.fromInt discards) ]
            ]
        , handElement deck selected
        , button [ css [ marginTop <| px 12 ], onClick Discard ] [ text "Discard" ]
        , Css.Global.global
            [ [ backgroundColor <| rgb 60 56 54
              ]
                |> Css.Global.body
            ]
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
                    [ paddingTop <| px 70
                    , displayFlex
                    , flexDirection row
                    , justifyContent center
                    , height <| px 170
                    , backgroundColor <| rgb 80 73 69
                    , borderRadius <| px 10
                    , boxShadow5 inset (px 0) (px 0) (px 10) (rgb 60 56 54)
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


cardElement : Msg -> Bool -> Card.Card -> Html Msg
cardElement msg selected card =
    div
        [ css
            [ position relative
            , bottom <|
                if selected then
                    px 70

                else
                    px 30
            ]
        ]
        [ button
            [ onClick msg
            , css
                [ fontSize <| rem 10
                , textAlign center
                , backgroundColor transparent
                , border (px 0)
                , let
                    ( r, g, b ) =
                        Card.suitToColor card.suit
                  in
                  color <| rgb r g b
                , hover
                    [ cursor pointer
                    ]
                ]
            ]
            [ text <|
                Card.cardToUnicode card
            ]
        ]
