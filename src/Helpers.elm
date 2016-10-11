module Helpers exposing (..)

import Dict
import Set
import Maybe exposing (withDefault)


count : a -> List a -> Int
count n l =
    l
        |> List.filter ((==) n)
        |> List.length


countAll : List comparable -> Dict.Dict comparable Int
countAll =
    let
        update : comparable -> Dict.Dict comparable Int -> Dict.Dict comparable Int
        update new current =
            let
                val =
                    (withDefault 0 (Dict.get new current)) + 1
            in
                Dict.insert new val current
    in
        List.foldl update Dict.empty


enumerate : List a -> List (Int, a)
enumerate =
    let
        -- f : (Int, a) -> a -> (Int, a)
        f e a =
            (List.length a, e) :: a
    in
        List.foldl f []
            >> List.reverse

toggle : comparable -> Set.Set comparable -> Set.Set comparable
toggle e s =
    let
        isMember =
            Set.member e s

        f =
            if isMember then
                Set.remove
            else
                Set.insert
    in
        f e s
