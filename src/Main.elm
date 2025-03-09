module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick)
import Model.Deck as Deck



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { selected : Bool
    , deck : Deck.Deck
    }


init : Model
init =
    { selected = True
    , deck = Deck.mkDeck ()
    }



-- UPDATE


type Msg
    = ToggleCardSelected
    | Draw


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleCardSelected ->
            { model | selected = model.selected }

        Draw ->
            { model | deck = Deck.draw 5 model.deck }



-- VIEW


view : Model -> Html Msg
view { deck } =
    Html.div []
        [ Html.input [ type_ "button", onClick Draw ] []
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
