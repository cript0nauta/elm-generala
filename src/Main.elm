module Main exposing (..)

import Dict
import Set
import Maybe exposing (Maybe(..), withDefault)
import Random
import Array
import List.Extra exposing (zip)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Helpers exposing (..)
import DibujarDados exposing (..)


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


generarDado : Random.Generator Int
generarDado = Random.int 1 6


generarDados : Int -> Random.Generator Dados
generarDados n = Random.list n generarDado


type alias Model =
    { puntos : Dict.Dict Categoria Int
    , dados : Dados
    , checked : Set.Set Int
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
        checked =
            -- Quiero que se renueven todos los dados
            Set.fromList [ 0, 1, 2, 3, 4 ]

        model =
            Model Dict.empty [0,0,0,0,0] checked 0
    in
        ( model
        , generarDados 5
            |> Random.generate ResultadoDados
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
    = ResultadoDados Dados
    | ToggleChecked Int
    | TirarDados


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultadoDados nuevosDados ->
            let
                viejosDados =
                    model.dados

                indexes =
                    Set.toList model.checked

                actualizaciones : List (Int, Int)  -- (index, dado)
                actualizaciones = zip indexes nuevosDados

                dados =
                    List.foldl
                        (uncurry Array.set)
                        (Array.fromList viejosDados)
                        actualizaciones
                    |> Array.toList

                debug =
                    (viejosDados, nuevosDados, dados)
                    |> Debug.log "Actualizando dados"
            in
                { model
                    | dados = dados
                    -- Dejo todos los checkboxes sin chequear
                    , checked = Set.empty} ! []

        ToggleChecked n ->
            let
                checked = toggle n model.checked
                    -- |> Debug.log "checked"
            in
                { model | checked = checked } ! []

        TirarDados ->
            let
                cantidad =
                    Set.size model.checked

                generador = generarDados cantidad
            in
                (model, (Random.generate ResultadoDados generador))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        containerDado (index, n) =
            div
                [ style [("float", "left")] ]
                [ dado aparienciaDefault n
                , br [] []
                , input
                    [ type' "checkbox" 
                    , checked (Set.member index model.checked)
                    , onClick (ToggleChecked index)]
                    [] ]
        dados = model.dados
            |> enumerate
            |> List.map containerDado
            |> div []
    in
        div []
            [ dados
            , input
                [ type' "submit"
                , onClick TirarDados
                , value "Tirar dados"] []
            ]
