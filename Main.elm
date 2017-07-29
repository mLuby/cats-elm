module Cats exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { pic : String
    , fact : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" ""
    , getPicAndFact
    )



-- UPDATE


type Msg
    = MorePlease
    | NewPic (Result Http.Error String)
    | NewFact (Result Http.Error String)



-- | NewPic (Result Http.Error String)
-- | NewFact (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getPicAndFact )

        NewPic (Ok newPic) ->
            ( { model | pic = newPic }, Cmd.none )

        NewPic (Err _) ->
            ( model, Cmd.none )

        NewFact (Ok newFact) ->
            ( { model | fact = newFact }, Cmd.none )

        NewFact (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Cats" ]
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


getPicAndFact : Cmd Msg
getPicAndFact =
    Cmd.batch [ getPic, getFact ]


getPic : Cmd Msg
getPic =
    Http.send NewPic picRequest


getFact : Cmd Msg
getFact =
    Http.send NewFact factRequest


picRequest : Http.Request String
picRequest =
    Http.get "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats" decodePic


factRequest : Http.Request String
factRequest =
    Http.get "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats" decodeFact



-- Http.get "https://api.flickr.com/services/rest/?&method=flickr.people.getPublicPhotos&format=json&api_key=6f93d9bd5fef5831ec592f0b527fdeff&user_id=9395899@N08" decodeFact
-- getFact : Cmd Msg
-- getFact =
--     let
--         url =
--             "https://catfact.ninja/fact"
--     in
--         Http.send NewPicAndFact (Http.get url decodeFact)


decodePic : Decode.Decoder String
decodePic =
    Decode.at [ "data", "image_url" ] Decode.string


decodeFact : Decode.Decoder String
decodeFact =
    Decode.at [ "data", "id" ] Decode.string
