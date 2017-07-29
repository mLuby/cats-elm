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
    { flash : String
    , cats : Cats
    }


init : ( Model, Cmd Msg )
init =
    ( Model "Fetching cats…" []
    , getCats
    )



-- UPDATE


type alias Cats =
    List Cat


type alias Cat =
    { fact : String, pic : String }


type Msg
    = AddCat (Result Http.Error String)
    | Flash String
    | FetchCat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Flash message ->
            ( { model | flash = message }, Cmd.none )

        FetchCat ->
            ( model, getCats )

        AddCat (Ok newPic) ->
            addCat model "fact" newPic

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
        , h2 [] [ text model.flash ]
        , button [ onClick (Flash "you flashed this") ] [ text "Flash a message" ]
        , button [ onClick FetchCat ] [ text "Add Cat" ]
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
