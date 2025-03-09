module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model.Deck as Deck
import Random



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
    { selected : Bool
    , deck : Maybe Deck.Deck
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = True
      , deck = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ToggleCardSelected
    | GenerateDeck
    | DeckGenerated Deck.Deck
    | Draw
    | Shuffle
    | Shuffled Deck.Deck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCardSelected ->
            ( { model | selected = not model.selected }, Cmd.none )

        Draw ->
            ( { model | deck = model.deck |> Maybe.map (Deck.draw 5) }, Cmd.none )

        GenerateDeck ->
            ( model, Random.generate DeckGenerated (Deck.mkDeck ()) )

        DeckGenerated deck ->
            ( { model | deck = Just deck }, Cmd.none )

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
        , Html.button [ onClick Shuffle ] [ text "Shuffle" ]
        , Html.button [ onClick GenerateDeck ] [ text "Generate" ]
        , handElement deck selected
        ]


handElement : Maybe Deck.Deck -> Bool -> Html Msg
handElement mDeck selected =
    case mDeck of
        Nothing ->
            div [] []

        Just deck ->
            Html.fieldset []
                (deck.hand
                    |> List.map (\card -> cardElement ToggleCardSelected selected card)
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
