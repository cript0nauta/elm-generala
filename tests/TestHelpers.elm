module TestHelpers exposing (..)

import Dict
import Test exposing (..)
import Expect
import Helpers exposing (..)


all : Test
all =
    describe "Helpers"
        [ describe "Count"
            [ test "Lista vacía = 0" <|
                \() ->
                    count "a" []
                        |> Expect.equal 0
            , test "Sin elementos que coinciden" <|
                \() ->
                    count 5 [ 1, 2, 3 ]
                        |> Expect.equal 0
            , test "Coinciden elementos" <|
                \() ->
                    count 2 [ 1, 2, 3, 2, 2 ]
                        |> Expect.equal 3
            ]
        , describe "countAll"
            [ test "Empty list" <|
                \() ->
                    countAll []
                        |> Expect.equal Dict.empty
            , test "Non-empty list" <|
                \() ->
                    countAll [ "a", "b", "c", "b", "b", "c", "b", "d" ]
                        |> Dict.toList
                        |> Expect.equal [ ( "a", 1 ), ( "b", 4 ), ( "c", 2 ), ( "d", 1 ) ]
            ]
        ]
