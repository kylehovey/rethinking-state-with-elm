module Model.Scoring exposing (..)

import GenericDict as Dict
import Model.Card as Card


type HandKind
    = HighCard
    | Pair
    | TwoPair
    | ThreeOfAKind
    | Straight
    | Flush
    | FullHouse
    | FourOfAKind
    | StraightFlush
    | RoyalFlush


handKindToString : Maybe HandKind -> String
handKindToString mHandKind =
    case mHandKind of
        Nothing ->
            "None"

        Just handKind ->
            case handKind of
                HighCard ->
                    "High Card"

                Pair ->
                    "Pair"

                TwoPair ->
                    "Two Pair"

                ThreeOfAKind ->
                    "Three of a Kind"

                Straight ->
                    "Straight"

                Flush ->
                    "Flush"

                FullHouse ->
                    "Full House"

                FourOfAKind ->
                    "Four of a Kind"

                StraightFlush ->
                    "Straight Flush"

                RoyalFlush ->
                    "Royal Flush"


groupBySuit : List Card.Card -> Dict.Dict Card.Suit (List Card.Card)
groupBySuit hand =
    hand
        |> List.map (\card -> ( card.suit, card ))
        |> List.foldl
            (\( suit, card ) acc ->
                case Dict.get Card.suitToString suit acc of
                    Nothing ->
                        Dict.insert Card.suitToString suit [ card ] acc

                    Just existing ->
                        Dict.insert Card.suitToString suit (List.append existing [ card ]) acc
            )
            Dict.empty


isFlush : List Card.Card -> Bool
isFlush hand =
    let
        suitCounts =
            groupBySuit hand
                |> Dict.toList
                |> List.map (List.length << Tuple.second)
    in
    List.any (\x -> x >= 5) suitCounts


getHandKind : List Card.Card -> Maybe HandKind
getHandKind hand =
    if isFlush hand then
        Just Flush

    else
        Nothing
