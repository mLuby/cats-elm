module Cats exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Cats =
    List Cat


type alias Cat =
    { fact : String, pic : String }



-- Redux state = Elm model.


type alias Model =
    { flash : String
    , cats : Cats
    }


initialModal : Model
initialModal =
    { flash = "Initializing…"
    , cats = []
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
    | FetchCatFail Http.Error
    | FetchCatSuccess Cat



-- Redux root reducer = Elm update. Note Elm runs side effects ¿BEFORE/AFTER?, called Commands)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Flash message ->
            ( { model | flash = message }, Cmd.none )

        RequestCat ->
            ( { model | flash = "Requesting cat…" }, getCat )

        FetchCatSuccess cat ->
            addCat model cat

        FetchCatFail error ->
            ( { model | flash = "fetch error" }, Cmd.none )


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



-- HTTP


getCat : Cmd Msg
getCat =
    Task.attempt parseResult
        (Task.map2
            (\fact pic -> { fact = fact, pic = pic })
            (Http.toTask (Http.get factGetUrl factDecoder))
            (Http.toTask (Http.get picGetUrl picDecoder))
        )


parseResult result =
    case result of
        Ok value ->
            FetchCatSuccess value

        Err msg ->
            FetchCatFail msg


factGetUrl =
    "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats"


factDecoder =
    Decode.at [ "data", "id" ] Decode.string


picGetUrl =
    "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats"


picDecoder =
    Decode.at [ "data", "image_url" ] Decode.string
