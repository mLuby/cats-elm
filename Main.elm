module Cats exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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
    { cats : Cats
    }


init : ( Model, Cmd Msg )
init =
    ( Model []
    , getCats
    )



-- UPDATE


type alias Cats =
    List Cat


type alias Cat =
    { fact : String, pic : String }


type Msg
    = AddCat (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCat (Ok newPic) ->
            addCat model newPic "foo"

        AddCat (Err _) ->
            ( model, Cmd.none )


addCat : Model -> String -> String -> ( Model, Cmd Msg )
addCat model newFact newPic =
    let
        newCat =
            { fact = newFact, pic = newPic }
    in
        ( { model | cats = model.cats ++ [ newCat ] }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Cats" ]
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


getCats : Cmd Msg
getCats =
    Cmd.batch [ getPic, getFact ]


getPicAndFact : Cmd Msg
getPicAndFact =
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
