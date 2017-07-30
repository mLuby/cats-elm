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
    | NewFace1 Int
    | NewFace2 Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Cmd.batch
                [ (Random.generate (NewFace1) (Random.int 1 6))
                , (Random.generate (NewFace2) (Random.int 1 6))
                ]
            )

        NewFace1 newFace ->
            ( { model | dice = (setListIndex 0 newFace model.dice) }, Cmd.none )

        NewFace2 newFace ->
            ( { model | dice = (setListIndex 1 newFace model.dice) }, Cmd.none )


setListIndex : Int -> a -> List a -> List a
setListIndex index newVal =
    List.indexedMap
        (\i oldVal ->
            if index == i then
                newVal
            else
                oldVal
        )



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
