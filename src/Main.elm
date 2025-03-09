module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick)
import Model.Deck as Deck
import Random



-- MAIN


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
    , deck : Deck.Deck
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = True
      , deck = Deck.draw 5 (Deck.mkDeck ())
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ToggleCardSelected
    | Draw
    | Shuffle
    | Shuffled Deck.Deck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCardSelected ->
            ( { model | selected = model.selected }, Cmd.none )

        Draw ->
            ( { model | deck = Deck.draw 5 model.deck }, Cmd.none )

        Shuffle ->
            ( model, Random.generate Shuffled (Deck.shuffle model.deck) )

        Shuffled newDeck ->
            ( { model | deck = newDeck }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { deck } =
    Html.div []
        [ Html.button [ onClick Draw ] [ text "Draw" ]
        , Html.button [ onClick Shuffle ] [ text "Shuffle" ]
        , Html.fieldset []
            (deck.hand
                |> List.map (\card -> cardElement ToggleCardSelected card)
            )
        ]


cardElement : Msg -> Deck.Card -> Html Msg
cardElement msg { suit, rank } =
    label
        [ style "padding" "20px" ]
        [ input [ type_ "checkbox", onClick msg ] []
        , text
            (String.join
                ""
                [ Deck.rankToString rank
                , " of "
                , Deck.suitToString suit
                ]
            )
        ]
