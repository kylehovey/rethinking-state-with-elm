module Model.Card exposing (..)

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


type alias Card =
    { id : UUID.UUID
    , suit : Suit
    , rank : Rank
    }


type SortOrder
    = BySuit
    | ByRank


sortHand : SortOrder -> List Card -> List Card
sortHand order =
    let
        comparison =
            case order of
                BySuit ->
                    suitToOrder << (\{ suit } -> suit)

                ByRank ->
                    rankToOrder << (\{ rank } -> rank)
    in
    List.sortBy comparison


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


suitToOrder : Suit -> Int
suitToOrder suit =
    let
        ( _, order, _ ) =
            suitMetadata suit
    in
    order


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


rankToOrder : Rank -> Int
rankToOrder rank =
    let
        ( _, order, _ ) =
            rankMetadata rank
    in
    order


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
