module Helpers exposing (..)

count : a -> List a -> Int
count n l =
    l
        |> List.filter ((==) n)
        |> List.length
