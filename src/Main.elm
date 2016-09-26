import Dict

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


-- MODEL

type alias Dados = List Int

type alias Model = 
    { puntos: Dict.Dict Categoria Int
    , dados: Dados
    , turno: Int
    }

type Categoria = Numero Int
    | Escalera
    | Full
    | Poker
    | Generala
    | DobleGenerala

init : (Model, Cmd Msg)
init = 
    let
        model = Model Dict.empty [] 0
    in
        ( model
        , Cmd.none)

puntaje : Categoria -> Dados -> Int
puntaje c d = 0


-- UPDATE

type Msg
    = A

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        A ->
            model ! []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    div [] []
