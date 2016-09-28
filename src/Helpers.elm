module Helpers exposing (..)

import Dict
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
