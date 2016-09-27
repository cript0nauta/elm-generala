module Puntaje exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import String

import Helpers
import Main exposing (puntaje, Categoria(..), Dados)


dado : Fuzzer Int
dado = Fuzz.intRange 1 6

dados : Fuzzer Dados
dados = Fuzz.list dado


all : Test
all =
    describe "Puntajes"
        [ describe "Jugar a un número"
            [ test "Jugarle al seis con tres seis es 36" <|
                \() ->
                    puntaje (Numero 6) [1,6,2,6,6]
                        |> Expect.equal 18
            , fuzz2 dado dados "Test genérico" <|
                \num all ->
                    all
                        |> puntaje (Numero num)
                        |> Expect.equal ((Helpers.count num all) * num)
            
            ]
        , describe "Escalera"
            [ test "1,2,3,4,5 da puntos" <|
                \() ->
                    [1..5]
                    |> puntaje Escalera
                    |> Expect.equal 20
            , test "Funciona con elementos desordenados" <|
                \() ->
                    [1,3,2,5,4]
                    |> puntaje Escalera
                    |> Expect.equal 20
            , test "Escalera al as" <|
                \() ->
                    [3,4,5,6,1]
                    |> puntaje Escalera
                    |> Expect.equal 20
            , test "Escalera intermedia" <|
                \() ->
                    [2,3,4,5,6]
                    |> puntaje Escalera
                    |> Expect.equal 20
            , test "0 puntos si tiene repetidos" <|
                \() ->
                    [1,1,2,3,4]
                    |> puntaje Escalera
                    |> Expect.equal 0
            , test "0 puntos si los elementos no son consecutivos" <|
                \() ->
                    [1,2,3,5,6]
                    |> puntaje Escalera
                    |> Expect.equal 0
            ]
        , describe "Full"
            [ test "Da puntos con un par y una terna" <|
                \() ->
                    [1,1,1,5,5]
                    |> puntaje Full
                    |> Expect.equal 30
            , test "Da puntos desordenado" <|
                \() ->
                    [2,1,1,2,1]
                    |> puntaje Full
                    |> Expect.equal 30
            , test "No da puntos con cinco iguales" <|
                \() ->
                    [1,1,1,1,1]
                    |> puntaje Full
                    |> Expect.equal 0
            , test "No da puntos con otra combinación" <|
                \() ->
                    [1,5,2,2,3]
                    |> puntaje Full
                    |> Expect.equal 0
            ]
        , describe "Poker"
            [ test "Da puntos con 4 iguales" <|
                \() ->
                    [5,3,5,5,5]
                    |> puntaje Poker
                    |> Expect.equal 40
            , test "Da puntos con 5 iguales" <| -- TODO: Ver si esto vale
                \() ->
                    [2,2,2,2,2]
                    |> puntaje Poker
                    |> Expect.equal 40
            , test "No da puntos con 3 iguales" <|
                \() ->
                    [1,1,1,2,2]
                    |> puntaje Poker
                    |> Expect.equal 0
            , test "No da puntos con otra combinación" <|
                \() ->
                    [3,5,3,2,1]
                    |> puntaje Poker
                    |> Expect.equal 0
            ]
        , describe "Generala"
            [ test "Da puntos con cinco cincos" <|
                \() ->
                    [5,5,5,5,5]
                    |> puntaje Generala
                    |> Expect.equal 50
            , test "Da puntos con cinco cuatros" <|
                \() ->
                    [4,4,4,4,4]
                    |> puntaje Generala
                    |> Expect.equal 50
            , test "No da puntos con cuatro iguales" <|
                \() ->
                    [4,4,3,4,4]
                    |> puntaje Generala
                    |> Expect.equal 0
            , test "No da puntos con otra combinación" <|
                \() ->
                    [1,5,3,4,3]
                    |> puntaje Generala
                    |> Expect.equal 0
            ]
        ]
