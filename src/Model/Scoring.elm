module Model.Scoring exposing (..)

import GenericDict as Dict
import Model.Card as Card
import Model.Util as Util


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
groupBySuit =
    Util.groupByKey (\{ suit } -> suit) Card.suitToString


groupByRank : List Card.Card -> Dict.Dict Card.Rank (List Card.Card)
groupByRank =
    Util.groupByKey (\{ rank } -> rank) Card.rankToString


hasFlush : List Card.Card -> Bool
hasFlush hand =
    let
        suitCounts =
            groupBySuit hand
                |> Dict.toList
                |> List.map (List.length << Tuple.second)
    in
    List.any (\x -> x >= 5) suitCounts


hasStraight : List Card.Card -> Bool
hasStraight hand =
    let
        sortedRanks =
            Card.sortHand Card.ByRank hand
                |> List.map (\{ rank } -> rank)

        handLength =
            List.length hand

        differences =
            sortedRanks
                |> List.map Card.rankToOrder
                |> Util.forwardDifference

        isHighStraight =
            sortedRanks == [ Card.Ace, Card.Five, Card.Four, Card.Three, Card.Two ]
    in
    (handLength == 5) && (List.all ((==) 1) differences || isHighStraight)


rankCounts : List Card.Card -> List Int
rankCounts hand =
    groupByRank hand
        |> Dict.toList
        |> List.map (List.length << Tuple.second)


hasNOfKind : Int -> List Card.Card -> Bool
hasNOfKind n hand =
    rankCounts hand
        |> List.any ((==) n)


hasTwoPair : List Card.Card -> Bool
hasTwoPair hand =
    let
        pairs =
            rankCounts hand
                |> List.filter ((==) 2)
    in
    List.length pairs == 2


hasFullHouse : List Card.Card -> Bool
hasFullHouse hand =
    let
        counts =
            rankCounts hand
    in
    List.member 3 counts && List.member 2 counts


hasRoyalStraight : List Card.Card -> Bool
hasRoyalStraight hand =
    let
        sortedOrder =
            Card.sortHand Card.ByRank hand
                |> List.map (\{ rank } -> rank)
    in
    sortedOrder == [ Card.Ace, Card.King, Card.Queen, Card.Jack, Card.Ten ]


getHandKind : List Card.Card -> Maybe HandKind
getHandKind hand =
    if hasFlush hand && hasRoyalStraight hand then
        Just RoyalFlush

    else if hasFlush hand && hasStraight hand then
        Just StraightFlush

    else if hasNOfKind 4 hand then
        Just FourOfAKind

    else if hasFullHouse hand then
        Just FullHouse

    else if hasFlush hand then
        Just Flush

    else if hasStraight hand then
        Just Straight

    else if hasNOfKind 3 hand then
        Just ThreeOfAKind

    else if hasTwoPair hand then
        Just TwoPair

    else if hasNOfKind 2 hand then
        Just Pair

    else if List.length hand > 0 then
        Just HighCard

    else
        Nothing
