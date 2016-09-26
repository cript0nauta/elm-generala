import Dict

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Helpers exposing (..)


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

{- Decide si se le deben asignar los puntos o no a una categoría -}
validar : Categoria -> Dados -> Bool
validar c d =
    case c of
        Escalera ->
            d
                |> List.sort
                |> flip List.member [[1,2,3,4,5], [2,3,4,5,6], [1,3,4,5,6]]
        _ ->
            False

{- Si la categoría es válida le da un puntaje fijo, sino es 0 -}
puntajeFijo : Int -> Categoria -> Dados -> Int
puntajeFijo p c d =
    if validar c d then
        p
    else
        0

puntaje : Categoria -> Dados -> Int
puntaje c d =
    case c of
        Numero n ->
            n * count n d

        Escalera ->
            puntajeFijo 20 c d
        _ ->
            Debug.crash "ok"


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
