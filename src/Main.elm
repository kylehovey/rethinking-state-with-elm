module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Css exposing (..)
import Css.Global
import EverySet
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)
import Model.Card as Card
import Model.Deck as Deck
import Model.Scoring as Scoring
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
    , sortOrder : Card.SortOrder
    , roundScore : Int
    , scoreToBeat : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = EverySet.empty
      , hands = 4
      , discards = 3
      , deck = Nothing
      , sortOrder = Card.ByRank
      , roundScore = 0
      , scoreToBeat = 300
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
    | PlayHand
    | Sort Card.SortOrder
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
            ( { model | deck = model.deck |> Maybe.map (Deck.draw cards model.sortOrder) }, Cmd.none )

        Sort sortOrder ->
            ( { model | sortOrder = sortOrder, deck = model.deck |> Maybe.map (Deck.sort sortOrder) }, Cmd.none )

        Discard ->
            case ( model.deck, model.discards, EverySet.size model.selected ) of
                ( _, _, 0 ) ->
                    ( model, Cmd.none )

                ( Nothing, _, _ ) ->
                    ( model, Cmd.none )

                ( _, 0, _ ) ->
                    ( model, Cmd.none )

                ( Just deck, discards, _ ) ->
                    ( { model
                        | deck = Just <| Deck.discard model.selected deck
                        , selected = EverySet.empty
                        , discards = discards - 1
                      }
                    , msgTask (Draw (EverySet.size model.selected))
                    )

        PlayHand ->
            case ( model.deck, model.hands, EverySet.size model.selected ) of
                ( _, _, 0 ) ->
                    ( model, Cmd.none )

                ( Nothing, _, _ ) ->
                    ( model, Cmd.none )

                ( _, 0, _ ) ->
                    ( model, Cmd.none )

                ( Just deck, hands, _ ) ->
                    let
                        score =
                            deck
                                |> Deck.getHand model.selected
                                |> Scoring.getHandScore
                    in
                    ( { model
                        | deck = Just <| Deck.discard model.selected deck
                        , selected = EverySet.empty
                        , roundScore = model.roundScore + score
                        , hands = hands - 1
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
view model =
    nav []
        [ div
            [ Attributes.css
                [ displayFlex
                , flexDirection column
                ]
            ]
            [ runInfo model
            , div
                [ Attributes.css
                    [ displayFlex
                    , flexDirection column
                    , alignItems center
                    , justifyContent flexEnd
                    , height <| vh 70
                    ]
                ]
                [ handElement model
                , playerControls model
                ]
            ]
        , Css.Global.global
            [ [ backgroundColor <| rgb 60 56 54
              , fontFamilies [ "Arial" ]
              , color <| rgb 235 219 178
              ]
                |> Css.Global.body
            ]
        ]


runInfo : Model -> Html Msg
runInfo ({ hands, discards } as model) =
    div
        [ Attributes.css
            [ displayFlex
            , flexDirection column
            , padding <| px 20
            , fontSize <| rem 1.2
            ]
        ]
        [ span [] [ text <| ("Hands: " ++ String.fromInt hands) ]
        , span [] [ text <| ("Discards: " ++ String.fromInt discards) ]
        , handInfoElement model
        ]


playerControls : Model -> Html Msg
playerControls model =
    div
        [ Attributes.css
            [ displayFlex
            ]
        ]
        [ bigButton "Play Hand"
            PlayHand
            [ Attributes.disabled <|
                EverySet.size model.selected
                    == 0
            ]
        , div
            [ Attributes.css
                [ displayFlex
                , flexDirection column
                , alignItems center
                , paddingLeft <| px 12
                , paddingRight <| px 12
                ]
            ]
            [ span
                [ Attributes.css
                    [ marginTop <| px 12
                    , marginBottom <| px 12
                    , fontWeight bold
                    ]
                ]
                [ text <| "Sort By" ]
            , div
                [ Attributes.css
                    [ displayFlex
                    , justifyContent spaceBetween
                    , width <| px 100
                    ]
                ]
                [ button [ onClick <| Sort Card.ByRank ] [ text "Rank" ]
                , button [ onClick <| Sort Card.BySuit ] [ text "Suit" ]
                ]
            ]
        , bigButton "Discard"
            Discard
            [ Attributes.disabled <|
                EverySet.size model.selected
                    == 0
            ]
        ]


bigButton : String -> Msg -> List (Attribute Msg) -> Html Msg
bigButton content msg attributes =
    button
        (List.append
            [ Attributes.css
                [ marginTop <| px 12
                , width <| px 80
                ]
            , onClick msg
            ]
            attributes
        )
        [ text content ]


handInfoElement : Model -> Html Msg
handInfoElement { deck, selected, roundScore, scoreToBeat } =
    let
        mHand =
            deck |> Maybe.map (Deck.getHand selected)

        mHandKind =
            mHand
                |> Maybe.andThen Scoring.parseHand
                |> Maybe.map (\{ kind } -> kind)

        { baseChips, baseMult } =
            mHandKind
                |> Maybe.map Scoring.getScoreInfoForKind
                |> Maybe.withDefault { baseChips = 0, baseMult = 0 }
    in
    div
        [ Attributes.css
            [ displayFlex
            , flexDirection column
            ]
        ]
        [ span [] [ text <| "Hand: " ++ Scoring.handKindToString mHandKind ]
        , span [] [ text <| "Round Score: " ++ String.fromInt roundScore ]
        , span [] [ text <| "Winning Score: " ++ String.fromInt scoreToBeat ]
        , span [] [ text <| String.fromInt baseChips ++ " x " ++ String.fromInt baseMult ]
        ]


handElement : Model -> Html Msg
handElement model =
    let
        isSelected card =
            EverySet.member card.id model.selected
    in
    case model.deck of
        Nothing ->
            div [] []

        Just deck ->
            div
                [ Attributes.css
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
        [ Attributes.css
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
            , Attributes.css
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
