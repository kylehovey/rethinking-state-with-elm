module Model.Card exposing (..)

import Css
import Model.Util as Util
import Style.Colors as Colors
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
                    cardToSuitOrder

                ByRank ->
                    cardToRankOrder
    in
    List.reverse
        << List.sortBy comparison


{-| (name, order, color)
-}
suitMetadata : Suit -> ( String, Int, Css.Color )
suitMetadata suit =
    case suit of
        Spade ->
            ( "Spades", 0, Colors.gray )

        Club ->
            ( "Clubs", 1, Colors.blue )

        Heart ->
            ( "Hearts", 2, Colors.red )

        Diamond ->
            ( "Diamonds", 3, Colors.yellow )


suitToString : Suit -> String
suitToString =
    Util.first3 << suitMetadata


suitToOrder : Suit -> Int
suitToOrder =
    Util.second3 << suitMetadata


suitToColor : Suit -> Css.Color
suitToColor =
    Util.third3 << suitMetadata


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
rankToString =
    Util.first3 << rankMetadata


rankToOrder : Rank -> Int
rankToOrder =
    Util.second3 << rankMetadata


rankToChips : Rank -> Int
rankToChips =
    Util.third3 << rankMetadata


{-| Order by rank alone, counting Ace as high
e.g. A,Q,J,4,3,2
-}
cardToRankOrder : Card -> Int
cardToRankOrder { rank } =
    let
        chips =
            rankToChips rank

        cardOrder =
            rankToOrder rank
    in
    chips * 100 + cardOrder


{-| Order by suit, and then by rank within suit
-}
cardToSuitOrder : Card -> Int
cardToSuitOrder ({ suit } as card) =
    let
        rankOrder =
            cardToRankOrder card

        suitOrder =
            suitToOrder suit
    in
    suitOrder * 10000 + rankOrder


{-| Knight is a card that is in the unicode block but that is
not in a standard 52-card deck.
-}
knightOfSuit : Suit -> Int
knightOfSuit suit =
    let
        ace =
            case suit of
                Spade ->
                    '🂬'

                Heart ->
                    '🂼'

                Diamond ->
                    '🃌'

                Club ->
                    '🃜'
    in
    Char.toCode ace


skipKnight : Suit -> Int -> Int
skipKnight suit code =
    let
        knightCode =
            knightOfSuit suit

        skip =
            if code >= knightCode then
                1

            else
                0
    in
    code + skip


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
        |> skipKnight suit
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
