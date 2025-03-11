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


type alias HandKindScore =
    { baseChips : Int
    , baseMult : Int
    }


getScoreInfoForKind : HandKind -> HandKindScore
getScoreInfoForKind kind =
    case kind of
        HighCard ->
            { baseChips = 5, baseMult = 1 }

        Pair ->
            { baseChips = 10, baseMult = 2 }

        TwoPair ->
            { baseChips = 20, baseMult = 2 }

        ThreeOfAKind ->
            { baseChips = 30, baseMult = 3 }

        Straight ->
            { baseChips = 30, baseMult = 4 }

        Flush ->
            { baseChips = 35, baseMult = 4 }

        FullHouse ->
            { baseChips = 40, baseMult = 4 }

        FourOfAKind ->
            { baseChips = 60, baseMult = 7 }

        StraightFlush ->
            { baseChips = 100, baseMult = 8 }

        RoyalFlush ->
            { baseChips = 100, baseMult = 8 }


getChipsInHand : List Card.Card -> Int
getChipsInHand hand =
    hand
        |> List.map (\{ rank } -> Card.rankToChips rank)
        |> List.sum


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


type alias ScoringHand =
    { kind : HandKind
    , scoringCards : List Card.Card
    }


type alias ScoringPredicate =
    List Card.Card -> Maybe ScoringHand


evalPredicate : ScoringPredicate -> List Card.Card -> Bool
evalPredicate predicate =
    Util.isJust << predicate


boolToMaybe : Bool -> a -> Maybe a
boolToMaybe value a =
    if value then
        Just a

    else
        Nothing


groupsOfRank : Int -> List Card.Card -> List (List Card.Card)
groupsOfRank n hand =
    groupByRank hand
        |> Dict.toList
        |> List.filter ((==) n << List.length << Tuple.second)
        |> List.map Tuple.second


firstGroupOfRank : Int -> List Card.Card -> Maybe (List Card.Card)
firstGroupOfRank n hand =
    groupsOfRank n hand
        |> List.head


parseFlush : ScoringPredicate
parseFlush hand =
    let
        suitCounts =
            groupBySuit hand
                |> Dict.toList
                |> List.map (List.length << Tuple.second)

        isFlush =
            List.any (\x -> x >= 5) suitCounts
    in
    boolToMaybe isFlush
        { kind = Flush
        , scoringCards = hand
        }


parseStraight : ScoringPredicate
parseStraight hand =
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

        isStraight =
            (handLength == 5) && (List.all ((==) 1) differences || isHighStraight)
    in
    boolToMaybe isStraight
        { kind = Straight
        , scoringCards = hand
        }


parseNOfKind : Int -> HandKind -> ScoringPredicate
parseNOfKind n kind hand =
    firstGroupOfRank n hand
        |> Maybe.map
            (\group ->
                { kind = kind
                , scoringCards = group
                }
            )


parsePair : ScoringPredicate
parsePair =
    parseNOfKind 2 Pair


parseThreeOfAKind : ScoringPredicate
parseThreeOfAKind =
    parseNOfKind 3 ThreeOfAKind


parseFourOfAKind : ScoringPredicate
parseFourOfAKind =
    parseNOfKind 4 FourOfAKind


parseTwoPair : ScoringPredicate
parseTwoPair _ =
    Nothing


parseFullHouse : ScoringPredicate
parseFullHouse hand =
    let
        mPair =
            firstGroupOfRank 2 hand

        mToak =
            firstGroupOfRank 3 hand

        makeScoringHand pair toak =
            { scoringCards = pair ++ toak
            , kind = FullHouse
            }
    in
    Maybe.map2 makeScoringHand mPair mToak


parseHighCard : ScoringPredicate
parseHighCard hand =
    let
        mHighCard =
            Card.sortHand Card.ByRank hand
                |> List.head
    in
    mHighCard
        |> Maybe.map
            (\card ->
                { kind = HighCard
                , scoringCards = [ card ]
                }
            )


parseStraightFlush : ScoringPredicate
parseStraightFlush hand =
    let
        isStraight =
            evalPredicate parseStraight hand

        isFlush =
            evalPredicate parseFlush hand
    in
    boolToMaybe (isStraight && isFlush)
        { kind = StraightFlush
        , scoringCards = hand
        }


parseRoyalFlush : ScoringPredicate
parseRoyalFlush hand =
    let
        sortedOrder =
            Card.sortHand Card.ByRank hand
                |> List.map (\{ rank } -> rank)

        isRoyalStraight =
            sortedOrder == [ Card.Ace, Card.King, Card.Queen, Card.Jack, Card.Ten ]

        isFlush =
            evalPredicate parseFlush hand
    in
    boolToMaybe (isFlush && isRoyalStraight)
        { kind = RoyalFlush
        , scoringCards = hand
        }


evalRules : List ScoringPredicate -> List Card.Card -> Maybe ScoringHand
evalRules rules hand =
    rules
        |> List.foldl
            (\predicate acc ->
                case acc of
                    Just _ ->
                        acc

                    Nothing ->
                        predicate hand
            )
            Nothing


scoringRules : List ScoringPredicate
scoringRules =
    [ parseRoyalFlush
    , parseStraightFlush
    , parseFourOfAKind
    , parseFullHouse
    , parseFlush
    , parseStraight
    , parseThreeOfAKind
    , parseTwoPair
    , parsePair
    , parseHighCard
    ]


parseHand : List Card.Card -> Maybe ScoringHand
parseHand =
    evalRules scoringRules


getHandScore : List Card.Card -> Int
getHandScore hand =
    case parseHand hand of
        Nothing ->
            0

        Just { kind, scoringCards } ->
            let
                { baseChips, baseMult } =
                    getScoreInfoForKind kind

                chipsInHand =
                    getChipsInHand scoringCards
            in
            (baseChips + chipsInHand) * baseMult
