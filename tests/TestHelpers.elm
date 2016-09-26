module TestHelpers exposing (..)

import Test exposing (..)
import Expect

import Helpers exposing (..)

all : Test
all = describe "Helpers"
    [ describe "Count" [
        test "Lista vacía = 0" <|
            \() ->
                count "a" []
                    |> Expect.equal 0
        , test "Sin elementos que coinciden" <|
            \() ->
                count 5 [1,2,3]
                    |> Expect.equal 0
        , test "Coinciden elementos" <|
            \() ->
                count 2 [1,2,3,2,2]
                    |> Expect.equal 3
        ]
    ]
