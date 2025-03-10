module Model.Deck exposing (..)

import EverySet
import Random
import Random.List as RandomList
import UUID


type Suit
    = Spade
    | Heart
    | Diamond
    | Club


type Rank
    = Ace
    | Two
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


{-| (name, order, color)
-}
suitMetadata : Suit -> ( String, Int, ( Int, Int, Int ) )
suitMetadata suit =
    case suit of
        Spade ->
            ( "Spades", 0, ( 50, 48, 39 ) )

        Club ->
            ( "Clubs", 1, ( 69, 133, 136 ) )

        Heart ->
            ( "Hearts", 2, ( 251, 73, 52 ) )

        Diamond ->
            ( "Diamonds", 3, ( 250, 189, 47 ) )


suitToString : Suit -> String
suitToString suit =
    let
        ( name, _, _ ) =
            suitMetadata suit
    in
    name


suitToColor : Suit -> ( Int, Int, Int )
suitToColor suit =
    let
        ( _, _, color ) =
            suitMetadata suit
    in
    color


{-| (name, order, score)
-}
rankMetadata : Rank -> ( String, Int, Int )
rankMetadata rank =
    case rank of
        Ace ->
            ( "Ace", 0, 11 )

        Two ->
            ( "Two", 1, 2 )

        Three ->
            ( "Three", 2, 3 )

        Four ->
            ( "Four", 3, 4 )

        Five ->
            ( "Five", 4, 5 )

        Six ->
            ( "Six", 5, 6 )

        Seven ->
            ( "Seven", 6, 7 )

        Eight ->
            ( "Eight", 7, 8 )

        Nine ->
            ( "Nine", 8, 9 )

        Ten ->
            ( "Ten", 9, 10 )

        Jack ->
            ( "Jack", 10, 10 )

        Queen ->
            ( "Queen", 11, 10 )

        King ->
            ( "King", 12, 10 )


rankToString : Rank -> String
rankToString rank =
    let
        ( name, _, _ ) =
            rankMetadata rank
    in
    name


{-| The first playing card in the Unicode block for each suit
-}
aceOfSuit : Suit -> Int
aceOfSuit suit =
    let
        ace =
            case suit of
                Spade ->
                    '🂡'

                Heart ->
                    '🂱'

                Diamond ->
                    '🃁'

                Club ->
                    '🃑'
    in
    Char.toCode ace


cardToUnicode : Card -> String
cardToUnicode { suit, rank } =
    let
        ( _, rankOrder, _ ) =
            rankMetadata rank
    in
    (aceOfSuit suit + rankOrder)
        |> Char.fromCode
        |> String.fromChar


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
    , handSize : Int
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


{-| Discard the selected cards from the current hand.
-}
discard : EverySet.EverySet UUID.UUID -> Deck -> Deck
discard selected ({ hand, discarded } as original) =
    let
        ( discardedCards, newHand ) =
            hand
                |> List.partition
                    (\{ id } ->
                        EverySet.member id selected
                    )
    in
    { original
        | hand = newHand
        , discarded = List.append discarded discardedCards
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
a word for it in Elm-land. Sequencing takes a nested structure
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


{-| This generates a basic 52 card deck as the product of all suits and ranks.
-}
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
                , handSize = 8
                }
            )
