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
    { dice = [ 1, 2, 3, 4, 5, 6 ] }


init : ( Model, Cmd Msg )
init =
    ( initialModal, Cmd.none )



-- Update


type Msg
    = Roll
    | NewFaceForIndex Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            model ! (List.indexedMap generateRandomFace model.dice)

        NewFaceForIndex index newFace ->
            { model | dice = (setValueAtIndex newFace index model.dice) } ! []


generateRandomFace : Int -> a -> Cmd Msg
generateRandomFace index _ =
    Random.generate (NewFaceForIndex index) (Random.int 1 6)


setValueAtIndex : a -> Int -> List a -> List a
setValueAtIndex newVal index =
    List.indexedMap
        (\i oldVal ->
            if index == i then
                newVal
            else
                oldVal
        )



-- View


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
