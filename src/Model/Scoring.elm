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


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair


forwardDifference : List Int -> List Int
forwardDifference values =
    let
        shifted =
            List.drop 1 values

        zipped =
            zip values shifted
    in
    zipped
        |> List.map (\( a, b ) -> a - b)


isStraight : List Card.Card -> Bool
isStraight hand =
    let
        sortedOrder =
            Card.sortHand Card.ByRank hand
                |> List.map (\{ rank } -> Card.rankToOrder rank)

        handLength =
            List.length hand

        differences =
            forwardDifference sortedOrder

        isHighStraight =
            sortedOrder == [ 11, 5, 4, 3, 2 ]
    in
    (handLength == 5) && (List.all (\x -> x == 1) differences || isHighStraight)


getHandKind : List Card.Card -> Maybe HandKind
getHandKind hand =
    if isFlush hand then
        Just Flush

    else if isStraight hand then
        Just Straight

    else
        Nothing
