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

type alias Model = 
    { someValue: String
    }

init : (Model, Cmd Msg)
init = 
    ( Model "complete me"
    , Cmd.none)


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
