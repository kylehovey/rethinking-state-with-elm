module Model.Deck exposing (..)

import EverySet
import GenericDict as Dict
import Model.Card as Card
import Random
import Random.List as RandomList
import UUID


type alias Deck =
    { hand : List Card.Card
    , deck : List Card.Card
    , discarded : List Card.Card
    , handSize : Int
    }


catMaybes : List (Maybe a) -> List a
catMaybes xs =
    xs
        |> List.foldl
            (\mx acc ->
                case mx of
                    Nothing ->
                        acc

                    Just x ->
                        List.append acc [ x ]
            )
            []


getHand : EverySet.EverySet UUID.UUID -> Deck -> List Card.Card
getHand selected deck =
    let
        cardPairs =
            deck.hand |> List.map (\card -> ( card.id, card ))

        handLookup =
            Dict.fromList UUID.toString cardPairs
    in
    EverySet.toList selected
        |> List.map (\cardId -> Dict.get UUID.toString cardId handLookup)
        |> catMaybes


draw : Int -> Card.SortOrder -> Deck -> Deck
draw amount sortOrder ({ hand, deck } as original) =
    let
        drawn =
            List.take amount deck

        newDeck =
            List.drop amount deck

        newHand =
            List.append hand drawn
    in
    { original
        | hand = Card.sortHand sortOrder newHand
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


mkCard : Card.Suit -> Card.Rank -> Random.Generator Card.Card
mkCard suit rank =
    UUID.generator
        |> Random.map
            (\uuid -> Card.Card uuid suit rank)


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
            Card.allSuits
                |> List.concatMap
                    (\suit ->
                        Card.allRanks
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
