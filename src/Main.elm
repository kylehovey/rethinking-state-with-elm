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
import Style.Colors as Colors
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


imports : List String
imports =
    [ "assets/balatro.otf" ]


view : Model -> Html Msg
view model =
    nav []
        [ div
            [ Attributes.css
                [ displayFlex
                , flexDirection row
                ]
            ]
            [ runInfo model
            , div
                [ Attributes.css
                    [ displayFlex
                    , flexDirection column
                    , alignItems center
                    , justifyContent flexStart
                    , height <| vh 100
                    , width <| pct 100
                    ]
                ]
                [ div
                    [ Attributes.css
                        [ displayFlex
                        , flexDirection column
                        , height <| vh 90
                        , justifyContent flexEnd
                        , alignItems center
                        ]
                    ]
                    [ handElement model
                    , playerControls model
                    ]
                ]
            ]
        , Css.Global.global
            [ [ backgroundColor Colors.bg
              , fontFamilies [ "Balatro" ]
              , color Colors.fg
              , letterSpacing <| px 1
              ]
                |> Css.Global.body
            ]
        , node "link" [ Attributes.href "src/fonts.css", Attributes.rel "stylesheet" ] []
        ]


runInfo : Model -> Html Msg
runInfo ({ hands, discards } as model) =
    div
        [ Attributes.css
            [ displayFlex
            , flexDirection column
            , padding <| px 20
            , fontSize <| rem 1.2
            , width <| px 350
            , alignItems center
            , backgroundColor Colors.bg1
            ]
        ]
        [ blindInfo model
        , roundScoreDisplay model
        , handScoreDisplay model
        , div
            [ Attributes.css
                [ displayFlex
                , flexDirection row
                , justifyContent spaceBetween
                , width <| pct 100
                ]
            ]
            [ numberCard "Hands" hands Colors.blueDim
            , numberCard "Discards" discards Colors.redDim
            ]
        ]


blindInfo : Model -> Html Msg
blindInfo model =
    div
        [ Attributes.css
            [ displayFlex
            , alignItems center
            , flexDirection column
            , width <| pct 100
            , height <| px 200
            , margin <| px 10
            ]
        ]
        [ div
            [ Attributes.css
                [ displayFlex
                , alignItems center
                , width <| pct 100
                , backgroundColor Colors.blue
                , height <| px 50
                , margin <| px 10
                , borderRadius <| px 5
                , justifyContent center
                ]
            ]
            [ span
                [ Attributes.css
                    [ textShadow3 (px 2) (px 2) Colors.bg2
                    , fontWeight bold
                    ]
                ]
                [ text "Small Blind" ]
            ]
        , div
            [ Attributes.css
                [ displayFlex
                , alignItems center
                , width <| pct 100
                , backgroundColor Colors.blueDim
                , height <| px 150
                , borderRadius <| px 5
                ]
            ]
            [ div
                [ Attributes.css
                    [ width <| px 80
                    , height <| px 80
                    , backgroundColor Colors.blue
                    , borderRadius <| px 80
                    , displayFlex
                    , alignItems center
                    , justifyContent center
                    , flexDirection column
                    , margin <| px 10
                    ]
                ]
                [ span
                    [ Attributes.css
                        [ textShadow3 (px 2) (px 2) Colors.bg2
                        ]
                    ]
                    [ text "Small" ]
                , span
                    [ Attributes.css
                        [ textShadow3 (px 2) (px 2) Colors.bg2
                        ]
                    ]
                    [ text "Blind" ]
                ]
            , div
                [ Attributes.css
                    [ backgroundColor Colors.bg3
                    , width <| pct 60
                    , height <| px 60
                    , displayFlex
                    , flexDirection column
                    , alignItems center
                    , justifyContent spaceAround
                    , borderRadius <| px 5
                    ]
                ]
                [ span [] [ text "Score at least" ]
                , span []
                    [ text <| "⛀ " ++ String.fromInt model.scoreToBeat ]
                ]
            ]
        ]


roundScoreDisplay : Model -> Html Msg
roundScoreDisplay model =
    div
        [ Attributes.css
            [ displayFlex
            , backgroundColor Colors.bg
            , width <| pct 100
            , borderRadius <| px 5
            , height <| px 80
            , justifyContent center
            , alignItems center
            ]
        ]
        [ div
            [ Attributes.css
                [ width <| px 50
                , displayFlex
                , flexDirection column
                , alignItems center
                ]
            ]
            [ span [] [ text "Round" ]
            , span [] [ text "Score" ]
            ]
        , div
            [ Attributes.css
                [ displayFlex
                , alignItems center
                , width <| pct 50
                , justifyContent flexEnd
                , backgroundColor Colors.bg2
                , marginLeft <| px 15
                , padding <| px 5
                , borderRadius <| px 5
                ]
            ]
            [ text <| "⛀ " ++ String.fromInt model.roundScore ]
        ]


handScoreDisplay : Model -> Html Msg
handScoreDisplay model =
    let
        mHand =
            model.deck |> Maybe.map (Deck.getHand model.selected)

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
            , width <| pct 100
            , backgroundColor Colors.bg
            , margin <| px 10
            , alignItems center
            , flexDirection column
            , borderRadius <| px 5
            , height <| px 100
            , justifyContent flexEnd
            ]
        ]
        [ span
            [ Attributes.css
                [ paddingTop <| px 10
                ]
            ]
            [ text <| Scoring.handKindToString mHandKind ]
        , div
            [ Attributes.css
                [ displayFlex
                , width <| pct 100
                , justifyContent center
                , alignItems center
                , paddingTop <| px 10
                , paddingBottom <| px 10
                ]
            ]
            [ div
                [ Attributes.css
                    [ displayFlex
                    , backgroundColor Colors.blueDim
                    , width <| pct 30
                    , borderRadius <| px 5
                    , justifyContent flexEnd
                    , padding <| px 5
                    ]
                ]
                [ span [] [ text <| String.fromInt baseChips ] ]
            , span
                [ Attributes.css
                    [ color Colors.redDim
                    , margin <| px 10
                    ]
                ]
                [ text "X" ]
            , div
                [ Attributes.css
                    [ displayFlex
                    , backgroundColor Colors.redDim
                    , width <| pct 30
                    , borderRadius <| px 5
                    , justifyContent flexStart
                    , padding <| px 5
                    ]
                ]
                [ span [] [ text <| String.fromInt baseMult ] ]
            ]
        ]


numberCard : String -> Int -> Color -> Html Msg
numberCard label n textColor =
    div
        [ Attributes.css
            [ displayFlex
            , flexDirection column
            , alignItems center
            , justifyContent center
            , width <| px 130
            , height <| px 80
            , backgroundColor Colors.bg
            , borderRadius <| px 5
            ]
        ]
        [ span
            [ Attributes.css
                [ marginBottom <| px 2
                ]
            ]
            [ text label ]
        , div
            [ Attributes.css
                [ width <| px 50
                , height <| px 40
                , borderRadius <| px 5
                , displayFlex
                , justifyContent center
                , alignItems center
                , backgroundColor Colors.bg2
                ]
            ]
            [ span
                [ Attributes.css
                    [ textShadow3 (px 2) (px 2) Colors.bg
                    , fontWeight bold
                    , color textColor
                    ]
                ]
                [ text <| String.fromInt n ]
            ]
        ]


playerControls : Model -> Html Msg
playerControls model =
    div
        [ Attributes.css
            [ displayFlex
            ]
        ]
        [ bigButton "Play Hand"
            Colors.blue
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
                [ text <| "Sort Hand" ]
            , div
                [ Attributes.css
                    [ displayFlex
                    , justifyContent spaceAround
                    , width <| px 100
                    ]
                ]
                [ button
                    [ Attributes.css
                        [ fontFamilies [ "Balatro" ]
                        , backgroundColor <| Colors.yellowDim
                        , border <| px 0
                        , borderRadius <| px 5
                        , padding <| px 5
                        , hover [ cursor pointer ]
                        , color Colors.fg
                        ]
                    , onClick <| Sort Card.ByRank
                    ]
                    [ text "Rank" ]
                , button
                    [ Attributes.css
                        [ fontFamilies [ "Balatro" ]
                        , backgroundColor <| Colors.yellowDim
                        , border <| px 0
                        , borderRadius <| px 5
                        , padding <| px 5
                        , hover [ cursor pointer ]
                        , color Colors.fg
                        ]
                    , onClick <| Sort Card.BySuit
                    ]
                    [ text "Suit" ]
                ]
            ]
        , bigButton "Discard"
            Colors.red
            Discard
            [ Attributes.disabled <|
                EverySet.size model.selected
                    == 0
            ]
        ]


bigButton : String -> Color -> Msg -> List (Attribute Msg) -> Html Msg
bigButton content bgColor msg attributes =
    button
        (List.append
            [ Attributes.css
                [ marginTop <| px 12
                , width <| px 80
                , backgroundColor bgColor
                , borderRadius <| px 5
                , border <| px 0
                , color Colors.fg
                , fontFamilies [ "Balatro" ]
                , hover
                    [ cursor pointer
                    ]
                ]
            , onClick msg
            ]
            attributes
        )
        [ text content ]


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
                    , backgroundColor Colors.bg2
                    , borderRadius <| px 10
                    , boxShadow5 inset (px 0) (px 0) (px 10) Colors.bg
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
                , color <| Card.suitToColor card.suit
                , hover
                    [ cursor pointer
                    ]
                ]
            ]
            [ text <|
                Card.cardToUnicode card
            ]
        ]
