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
    describe "Jugar a un número"
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
