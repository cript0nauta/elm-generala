port module TestsMain exposing (..)

import Puntaje
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Value
main =
    run emit Puntaje.all


port emit : ( String, Value ) -> Cmd msg
