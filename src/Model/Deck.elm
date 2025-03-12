module Model.Deck exposing (..)

import EverySet
import GenericDict as Dict
import Model.Card as Card
import Model.Util as Util
import Random
import Random.List as RandomList
import UUID


type alias Deck =
    { hand : List Card.Card
    , deck : List Card.Card
    , discarded : List Card.Card
    , handSize : Int
    }


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
        |> Util.catMaybes


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


sort : Card.SortOrder -> Deck -> Deck
sort sortOrder ({ hand } as original) =
    { original | hand = Card.sortHand sortOrder hand }


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


{-| This generates a basic 52 card deck as the product of all suits and ranks.
-}
mkDeck : Random.Generator Deck
mkDeck =
    let
        cardGen =
            Card.allSuits
                |> List.concatMap
                    (\suit ->
                        Card.allRanks
                            |> List.map (\rank -> mkCard suit rank)
                    )
    in
    Util.sequenceRandom cardGen
        |> Random.map
            (\deck ->
                { hand = []
                , deck = deck
                , discarded = []
                , handSize = 8
                }
            )
