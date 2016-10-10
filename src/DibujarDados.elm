module DibujarDados exposing (Apariencia, aparienciaDefault, dado)

import Html exposing (..)
import Svg
import Svg.Attributes exposing (..)

type Posicion = Inicio | Centro | Fin

type alias Apariencia =
    { ancho: Int
    , margen: Int
    , radio: Int
    , color: String
    }

aparienciaDefault : Apariencia
aparienciaDefault =
    { ancho = 110
    , margen = 25
    , radio = 10
    , color = "black"}


{-| Posiciones en la que se encuentra cada número en el
dado
-}

posiciones : Int -> List (Posicion, Posicion)
posiciones n =
    let
        -- partes par e impar
        impar = rem n 2
        par = n - impar
        centro =
            if impar == 1 then
                [ (Centro, Centro) ]
            else
                []
        bordes = List.take par
            [ (Fin, Inicio)
            , (Inicio, Fin)
            , (Inicio, Inicio)
            , (Fin, Fin)
            , (Inicio, Centro)
            , (Fin, Centro)
            ]
    in
        bordes ++ centro


{-| Convertir coordenadas relativas (de posición) en absolutas
utilizando la configuración del dado (apariencia)
-}

coordenadas : Apariencia -> (Posicion, Posicion) -> (Int, Int)
coordenadas apariencia (x, y) =
    let
        coordenada : Posicion -> Int
        coordenada p =
            case p of
                Centro ->
                    apariencia.ancho // 2

                Inicio ->
                    apariencia.margen

                Fin ->
                    apariencia.ancho - apariencia.margen
    in
        (coordenada x, coordenada y)



{-| Función principal del módulo
-}
dado : Apariencia -> Int -> Html msg
dado apariencia n =
    let
        ancho = toString apariencia.ancho
        anchoContainer = toString (apariencia.ancho + 10)
        -- Convertir una coordenada a un círculo de radio definido en la apariencia
        circulo : (Int, Int) -> Svg.Svg msg
        circulo (x, y) =
            let
                x' = toString x
                y' = toString y
            in
                Svg.circle
                    [ cx x'
                    , cy y'
                    , r (toString apariencia.radio)
                    , fill apariencia.color
                    ]
                    [ ]

        circulos = n
            |> posiciones
            |> List.map (coordenadas apariencia)
            |> List.map circulo

        rect =
            Svg.rect 
                [ width ancho
                , height ancho
                , fill "none" 
                , stroke apariencia.color
                , strokeWidth "5"]
                []
    in
        Svg.svg [ width anchoContainer, height anchoContainer ] (rect :: circulos)
