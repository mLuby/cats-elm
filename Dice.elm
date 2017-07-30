module Dice exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { dice : List Int }


initialModal : Model
initialModal =
    { dice = [ 4, 2 ] }


init : ( Model, Cmd Msg )
init =
    ( initialModal, Cmd.none )



-- Update


type Msg
    = Roll
    | NewFaces List Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Cmd.batch (
                  List.map rollDie model.dice
              )
            )

        NewFaces newFaces ->
            ( { model | dice = newFaces }, Cmd.none )

rollDie : Cmd msg
rollDie =
  Random.generate NewFaces (Random.int 1 6)

-- VIEW


view : Model -> Html Msg
view model =
    div [] (List.map renderDie model.dice)


renderDie : Int -> Html Msg
renderDie face =
    img
        [ onClick Roll
        , src ("https://wpclipart.com/recreation/games/dice/die_face_" ++ (toString face) ++ ".png")
        , width 50
        ]
        []
