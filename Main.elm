module Cats exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init "cats"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { topic : String
    , pic : String
    , fact : String
    }


init : String -> ( Model, Cmd Msg )
init topic =
    ( Model topic "waiting.gif" "<cat fact>"
    , getPic topic
    )



-- UPDATE


type Msg
    = MorePlease
    | NewPic (Result Http.Error String)
    | NewFact (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getPic model.topic )

        NewPic (Ok newPic) ->
            ( { model | pic = newPic }, Cmd.none )

        NewPic (Err _) ->
            ( model, Cmd.none )

        NewFact (Ok newFact) ->
            ( model, Cmd.none )

        NewFact (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.topic ]
        , button [ onClick MorePlease ] [ text "More Purrease!" ]
        , br [] []
        , img [ src model.pic ] []
        , br [] []
        , span [] [ text model.fact ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP
-- https://catfact.ninja/fact


getPic : String -> Cmd Msg
getPic topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
        Http.send NewPic (Http.get url decodePic)


decodePic : Decode.Decoder String
decodePic =
    Decode.at [ "data", "image_url" ] Decode.string


getFact : String -> Cmd Msg
getFact topic =
    let
        url =
            "https://catfact.ninja/fact"
    in
        Http.send NewFact (Http.get url decodeFact)


decodeFact : Decode.Decoder String
decodeFact =
    Decode.at [ "fact" ] Decode.string
