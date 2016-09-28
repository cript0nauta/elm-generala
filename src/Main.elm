module Main exposing (..)

import Dict
import Maybe exposing (Maybe(..))
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


type alias Dados =
    List Int


type alias Model =
    { puntos : Dict.Dict Categoria Int
    , dados : Dados
    , turno : Int
    }


type Categoria
    = Numero Int
    | Escalera
    | Full
    | Poker
    | Generala
    | DobleGenerala


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model Dict.empty [] 0
    in
        ( model
        , Cmd.none
        )



{- Decide si se le deben asignar los puntos o no a una categoría -}


validar : Categoria -> Dados -> Bool
validar c d =
    let
        {- Retornar el elemento más veces repetido si la lista tiene elementos -}
        maxRepetitions : List comparable -> Maybe comparable
        maxRepetitions =
            countAll
                >> Dict.values
                >> List.sort
                >> List.reverse
                >> List.head

        maybeFilter : (a -> Bool) -> Maybe a -> Bool
        maybeFilter f =
            Maybe.map f
                >> Maybe.withDefault False
    in
        case c of
            Escalera ->
                d
                    |> List.sort
                    |> flip List.member [ [ 1, 2, 3, 4, 5 ], [ 2, 3, 4, 5, 6 ], [ 1, 3, 4, 5, 6 ] ]

            Full ->
                d
                    |> countAll
                    |> Dict.values
                    |> List.sort
                    |> (==) [ 2, 3 ]

            Poker ->
                d
                    |> maxRepetitions
                    |> maybeFilter (flip (>=) 4)

            Generala ->
                d
                    |> maxRepetitions
                    |> maybeFilter ((==) 5)

            _ ->
                False



{- Si la categoría es válida le da un puntaje fijo, sino es 0 -}


puntajeFijo : Categoria -> Dados -> Int -> Int
puntajeFijo c d p =
    if validar c d then
        p
    else
        0


puntaje : Categoria -> Dados -> Int
puntaje c d =
    let
        fijo : Int -> Int
        fijo =
            puntajeFijo c d
    in
        case c of
            Numero n ->
                n * count n d

            Escalera ->
                fijo 20

            Full ->
                fijo 30

            Poker ->
                fijo 40

            Generala ->
                fijo 50

            DobleGenerala ->
                puntajeFijo Generala d 100



-- UPDATE


type Msg
    = A


update : Msg -> Model -> ( Model, Cmd Msg )
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
