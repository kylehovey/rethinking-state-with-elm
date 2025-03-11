module Model.Util exposing (..)

import GenericDict as Dict
import Random



-- TUPLE


first3 : ( a, b, c ) -> a
first3 ( a, _, _ ) =
    a


second3 : ( a, b, c ) -> b
second3 ( _, b, _ ) =
    b


third3 : ( a, b, c ) -> c
third3 ( _, _, c ) =
    c



-- MAYBE


{-| Given a list of Maybe values, return a new list containing
all of the Just values
-}
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



-- DICT


groupByKey : (a -> b) -> (b -> String) -> List a -> Dict.Dict b (List a)
groupByKey getKey keyToString elems =
    elems
        |> List.map (\elem -> ( getKey elem, elem ))
        |> List.foldl
            (\( key, elem ) acc ->
                case Dict.get keyToString key acc of
                    Nothing ->
                        Dict.insert keyToString key [ elem ] acc

                    Just existing ->
                        Dict.insert keyToString key (List.append existing [ elem ]) acc
            )
            Dict.empty



-- LIST


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



-- RANDOM


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
