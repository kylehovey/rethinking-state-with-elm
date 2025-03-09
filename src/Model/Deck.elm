module Model.Deck exposing (..)

import Random
import Random.List as RandomList
import UUID


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
    { id : UUID.UUID
    , suit : Suit
    , rank : Rank
    }


type alias Deck =
    { hand : List Card
    , deck : List Card
    , discarded : List Card
    , discards : Int
    , hands : Int
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


shuffle : Deck -> Random.Generator Deck
shuffle ({ deck } as original) =
    RandomList.shuffle deck
        |> Random.map
            (\shuffled -> { original | deck = shuffled })


mkCard : Suit -> Rank -> Random.Generator Card
mkCard suit rank =
    UUID.generator
        |> Random.map
            (\uuid -> Card uuid suit rank)


{-| This is a really common operation in Haskell, but we don't have
a word for it in Elm-land. Traversal takes a nested structure
and collects the outer structure, then places the results in
the inner structure. In this case, it's collapsing a list of
eventual random results into one random result of a list.

The overall benefit: we don't have to do `n` random calls, just one.

In Haskell, this is often used to collect multiple database queries
into one query (e.g. users <- sequence userQueries).

In Elm, we need to define this for each pair of structures as we cannot
generally type `sequence` without Higher Kinded Types. The type of sequence
in Haskell is:

sequence :: (Monad m) => t (m a) -> m (t a)

-}
sequenceRandom : List (Random.Generator a) -> Random.Generator (List a)
sequenceRandom =
    List.foldr
        (Random.map2 (::))
        (Random.constant [])


mkDeck : () -> Random.Generator Deck
mkDeck _ =
    let
        cardGen =
            allSuits
                |> List.concatMap
                    (\suit ->
                        allRanks
                            |> List.map (\rank -> mkCard suit rank)
                    )
    in
    sequenceRandom cardGen
        |> Random.map
            (\deck ->
                { hand = []
                , deck = deck
                , discarded = []
                , discards = 3
                , hands = 4
                }
            )
