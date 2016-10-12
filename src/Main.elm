module Main exposing (..)

import Dict
import Set
import Maybe exposing (Maybe(..), withDefault)
import Random
import Array
import List.Extra exposing (zip, (!!))

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
    -- { puntos : Dict.Dict Categoria Int
    -- Uso enteros como keys de puntos porque Categoria no es comparable
    { puntos : Dict.Dict Int Int
    , dados : Dados
    , checked : Set.Set Int
    , intento : Int
    }


type Categoria
    = Numero Int
    | Escalera
    | Full
    | Poker
    | Generala
    | DobleGenerala


categorias : List Categoria
categorias =
    (List.map Numero [1..6]) ++
        [ Escalera
        , Full
        , Poker
        , Generala
        , DobleGenerala ]
    |> Debug.log "Categorias"


allChecked = Set.fromList [ 0, 1, 2, 3, 4 ]


nuevoTurno : Cmd Msg
nuevoTurno =
    generarDados 5
        |> Random.generate ResultadoDados


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model Dict.empty [0,0,0,0,0] allChecked 0
    in
        ( model
        , nuevoTurno
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
    | JugarCategoria Categoria


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
                    , intento = (model.intento + 1 |> Debug.log "update intento")
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

        JugarCategoria c ->
            let
                index = List.Extra.elemIndex c categorias
                    |> withDefault 0
                puntos = Dict.insert index (puntaje c model.dados) model.puntos
                    |> Debug.log "update puntos"
            in
                { model
                    | puntos = puntos
                    , checked = allChecked
                    , intento = 0 } ! [ nuevoTurno ]


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
                    , disabled ultimoIntento
                    , onClick (ToggleChecked index)]
                    [] ]
        dados = model.dados
            |> enumerate
            |> List.map containerDado
            |> div []

        ultimoIntento = model.intento >= 3

        puedeTirar =
            (not (Set.isEmpty model.checked)) && (not ultimoIntento)

        seJugo : Categoria -> Bool
        seJugo c =
            let
                encontrado = List.Extra.elemIndex c categorias
                    |> flip Maybe.andThen (flip Dict.get model.puntos)
            in
                case encontrado of
                    Just _ ->
                        True
                    Nothing ->
                        False

        botonesCategoria =
            categorias
                |> List.map
                    (\c -> button
                        [ onClick <| JugarCategoria c
                        , disabled (seJugo c)]
                        [ text <| toString c ])

        puntajes =
            let
                toTd e = td [] [ text e ]
                size = List.length categorias

                tabla : List (String, String)
                tabla =
                    let
                        categoria i =
                            categorias !! i
                                |> Maybe.map toString
                                |> withDefault "?"
                        puntaje' i =
                            Dict.get i model.puntos
                                |> Maybe.map toString
                                |> withDefault ""
                    in
                        [0..(size-1)]
                            |> List.map (\i -> (categoria i, puntaje' i))
            in
                tabla
                    |> List.map (\(a,b) -> [ a, b ] |> List.map toTd)
                    |> List.map (tr [])
                    |> table []
                -- model.puntos
                --     |> Dict.values
                --     |> zip categorias
                --     |> List.map (\(c, p) -> [ toTd c, toTd p ])
                --     |> List.map (tr [])
                --     |> table []
                -- [0..size]
                --     -- Convertir
                --     |> List.map (flip Dict.get model.puntos)
    in
        div []
            [ dados
            , input
                [ type' "submit"
                , onClick TirarDados
                , disabled (not puedeTirar)
                , value "Tirar dados"] []
            , br [] []
            , div [] botonesCategoria
            , puntajes
            ]
