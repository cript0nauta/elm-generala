import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Svg
import DibujarDados exposing (..)


main =
    App.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }


-- MODEL

type alias Model = 
    { dados: List Int
    , apariencia: Apariencia
    }

initialModel : Model
initialModel = Model
    [ 1,2,3,4,5,6 ]
    aparienciaDefault


-- UPDATE

type Msg
    = A

update : Msg -> Model -> Model
update msg model =
    case msg of
        A ->
            model


-- VIEW
view : Model -> Html Msg
view model =
    model.dados
        |> List.map (dado model.apariencia)
        |> div []  -- Lo meto todo en un div

