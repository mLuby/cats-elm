module Cats exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Cats =
    List Cat


type alias Cat =
    { fact : String, pic : String }



-- Redux state = Elm model.


type alias Model =
    { flash : String
    , cats : Cats
    , die1 : Int
    , die2 : Int
    , die3 : Int
    }


initialModal : Model
initialModal =
    { flash = "Initializing…"
    , cats = []
    , die1 = 1
    , die2 = 2
    , die3 = 3
    }


init : ( Model, Cmd Msg )
init =
    ( initialModal
    , Cmd.batch [ getCat, getCat ]
    )



-- Redux actions = Elm Msg. Types specifiy actions and their payloads (if any).


type Msg
    = Flash String
    | RequestCat
    | AddCat (Result Http.Error String)
    | Roll
    | NewFace1 Int
    | NewFace2 Int
    | NewFace3 Int



-- Redux root reducer = Elm update. Note Elm runs side effects BEFORE/AFTER?, called Commands)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Flash message ->
            ( { model | flash = message }, Cmd.none )

        RequestCat ->
            ( { model | flash = "Requesting cat…" }, getCat )

        AddCat (Ok str) ->
            addCat model { pic = str, fact = "foo" }

        AddCat (Err _) ->
            ( model, Cmd.none )

        Roll ->
            ( model
            , Cmd.batch
                [ Random.generate NewFace1 (Random.int 1 6)
                , Random.generate NewFace2 (Random.int 1 6)
                , Random.generate NewFace3 (Random.int 1 6)
                ]
            )

        NewFace1 newFace ->
            ( { model | die1 = newFace }, Cmd.none )

        NewFace2 newFace ->
            ( { model | die2 = newFace }, Cmd.none )

        NewFace3 newFace ->
            ( { model | die3 = newFace }, Cmd.none )


addCat : Model -> Cat -> ( Model, Cmd Msg )
addCat model cat =
    ( { model
        | cats = model.cats ++ [ cat ]
        , flash = "Success!"
      }
    , Cmd.none
    )



-- React = Elm view.


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.flash ]
        , div []
            [ img [ onClick Roll, src ("https://wpclipart.com/recreation/games/dice/die_face_" ++ (toString model.die1) ++ ".png"), width 50 ] []
            , img [ onClick Roll, src ("https://wpclipart.com/recreation/games/dice/die_face_" ++ (toString model.die2) ++ ".png"), width 50 ] []
            , img [ onClick Roll, src ("https://wpclipart.com/recreation/games/dice/die_face_" ++ (toString model.die3) ++ ".png"), width 50 ] []
            ]
        , h1 [] [ text "Cats" ]
        , button [ onClick (Flash "you flashed this") ] [ text "Flash a message" ]
        , button [ onClick RequestCat ] [ text "Add Cat" ]
        , (renderCats model.cats)
        ]


renderCats : Cats -> Html Msg
renderCats cats =
    ul [] (List.map renderCat cats)


renderCat : Cat -> Html Msg
renderCat cat =
    li []
        [ img [ src cat.pic ] []
        , span [] [ text cat.fact ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP
-- getCat : Cmd Msg
-- getCat =
--   Task.perform Flash Cat
--     <| Task.map2 (\fact pic -> { fact = fact, pic = pic })
--         factRequest
--         picRequest


getCat : Cmd Msg
getCat =
    Cmd.batch [ getPic, getFact ]


getPic : Cmd Msg
getPic =
    Http.send AddCat picRequest


getFact : Cmd Msg
getFact =
    Http.send AddCat factRequest


picRequest : Http.Request String
picRequest =
    Http.get "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats" decodePic


factRequest : Http.Request String
factRequest =
    Http.get "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats" decodeFact


decodePic : Decode.Decoder String
decodePic =
    Decode.at [ "data", "image_url" ] Decode.string


decodeFact : Decode.Decoder String
decodeFact =
    Decode.at [ "data", "id" ] Decode.string
