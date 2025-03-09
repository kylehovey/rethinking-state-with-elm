module Model.Deck exposing (..)


type Suit
    = Spade
    | Club
    | Heart
    | Diamond


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


suitToString : Suit -> String
suitToString suit =
    case suit of
        Spade ->
            "Spades"

        Club ->
            "Clubs"

        Heart ->
            "Hearts"

        Diamond ->
            "Diamonds"


rankToString : Rank -> String
rankToString rank =
    case rank of
        Two ->
            "Two"

        Three ->
            "Three"

        Four ->
            "Four"

        Five ->
            "Five"

        Six ->
            "Six"

        Seven ->
            "Seven"

        Eight ->
            "Eight"

        Nine ->
            "Nine"

        Ten ->
            "Ten"

        Jack ->
            "Jack"

        Queen ->
            "Queen"

        King ->
            "King"

        Ace ->
            "Ace"


allSuits : List Suit
allSuits =
    [ Spade, Club, Heart, Diamond ]


allRanks : List Rank
allRanks =
    [ Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Jack
    , Queen
    , King
    , Ace
    ]


type alias Card =
    { suit : Suit
    , rank : Rank
    }


type alias Deck =
    { hand : List Card
    , deck : List Card
    , discarded : List Card
    , discards : Int
    , hands : Int
    }


mkDeck : () -> Deck
mkDeck _ =
    let
        deck =
            allSuits
                |> List.concatMap
                    (\suit ->
                        allRanks
                            |> List.map (\rank -> Card suit rank)
                    )
    in
    { hand = []
    , deck = deck
    , discarded = []
    , discards = 3
    , hands = 4
    }


draw : Int -> Deck -> Deck
draw amount ({ hand, deck } as original) =
    let
        drawn =
            List.take amount deck

        newDeck =
            List.drop amount deck

        newHand =
            List.append hand drawn
    in
    { original
        | hand = newHand
        , deck = newDeck
    }


enumerate : List a -> List ( Int, a )
enumerate =
    List.indexedMap Tuple.pair


snd : ( a, b ) -> b
snd ( _, y ) =
    y


discard : List Int -> Deck -> Deck
discard selected ({ hand, discarded, discards } as original) =
    if discards < 1 then
        original

    else
        let
            ( inDiscard, inHand ) =
                enumerate hand
                    |> List.partition
                        (\( order, _ ) ->
                            List.member order selected
                        )

            discardedCards =
                inDiscard |> List.map snd

            newHand =
                inHand |> List.map snd
        in
        { original
            | hand = newHand
            , discarded = List.append discarded discardedCards
            , discards = discards - 1
        }
