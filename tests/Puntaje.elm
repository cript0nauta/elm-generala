module Puntaje exposing (..)

import Test exposing (..)
import Expect
import String

import Main exposing (puntaje, Categoria(..))


all : Test
all =
    describe "Jugar a un número"
        [ test "Jugarle al seis con tres seis es 36" <|
            \() ->
                puntaje (Numero 6) [1,6,2,6,6]
                    |> Expect.equal 18
        ]
