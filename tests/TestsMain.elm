port module TestsMain exposing (..)

import Puntaje
import TestHelpers
import Test.Runner.Node exposing (run)
import Test exposing (describe)
import Json.Encode exposing (Value)


main : Program Value
main =
    let
        tests =
            [ Puntaje.all
            , TestHelpers.all
            ]
    in
    run emit (describe "General" tests)


port emit : ( String, Value ) -> Cmd msg
