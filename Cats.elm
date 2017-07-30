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
        { init = ( initialModal, Cmd.batch [ getCat 0, getCat 1 ] )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Redux state = Elm model.


type alias Model =
    { flash : String
    , cats : List Cat
    }


initialModal : Model
initialModal =
    { flash = "Initializing…"
    , cats = [ createCat 0 2, createCat 1 2 ]
    }


type alias Cat =
    { id : Int, stillLoading : Int, fact : String, pic : String }



-- Redux actions = Elm Msg. Types specifiy actions and their payloads (if any).


type Msg
    = Flash String
    | RequestParallelCat
    | ReceivePartialCat (Cat -> Cat)
    | FetchCatFail Http.Error



-- Redux root reducer = Elm update. Note Elm runs side effects ¿BEFORE/AFTER?, called Commands)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Flash message ->
            ( { model | flash = message }, Cmd.none )

        RequestParallelCat ->
            ( { model | flash = "Requesting parallel cat…" }, Cmd.none )

        ReceivePartialCat catUpdater ->
            ( { model | cats = List.map catUpdater model.cats }, Cmd.none )

        FetchCatFail error ->
            ( { model | flash = "fetch error" }, Cmd.none )


createCat : a -> b -> { fact : String, id : a, pic : String, stillLoading : b }
createCat catId requiredRequestCount =
    { id = catId, stillLoading = requiredRequestCount, fact = "", pic = "" }



-- React render = Elm view.


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.flash ]
        , h1 [] [ text "Cats" ]
        , button [ onClick (Flash "you flashed this") ] [ text "Flash a message" ]
        , button [ onClick RequestParallelCat ] [ text "Add Cat" ]
        , (renderCats model.cats)
        ]


renderCats : List Cat -> Html Msg
renderCats cats =
    ul [] (List.map renderCat cats)


renderCat : Cat -> Html Msg
renderCat cat =
    li [ hidden (cat.stillLoading > 0) ]
        [ img [ src cat.pic ] []
        , span [] [ text cat.fact ]
        ]



-- HTTP


getCat : Int -> Cmd Msg
getCat catId =
    Cmd.batch [ getCatFact catId, getCatPic catId ]


getCatFact : Int -> Cmd Msg
getCatFact catId =
    Task.attempt parseHttpResult
        (Task.map
            (\fact ->
                (\cat ->
                    if cat.id == catId then
                        { cat
                            | fact = fact
                            , stillLoading = cat.stillLoading - 1
                        }
                    else
                        cat
                )
            )
            (Http.toTask (Http.get factUrl factDecoder))
        )


getCatPic : Int -> Cmd Msg
getCatPic catId =
    Task.attempt parseHttpResult
        (Task.map
            (\pic ->
                (\cat ->
                    if cat.id == catId then
                        { cat | pic = pic, stillLoading = cat.stillLoading - 1 }
                    else
                        cat
                )
            )
            (Http.toTask (Http.get picUrl picDecoder))
        )


parseHttpResult : Result Http.Error (Cat -> Cat) -> Msg
parseHttpResult result =
    case result of
        Ok catUpdater ->
            ReceivePartialCat catUpdater

        Err msg ->
            FetchCatFail msg


factUrl : String
factUrl =
    "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats"


factDecoder : Decode.Decoder String
factDecoder =
    Decode.at [ "data", "id" ] Decode.string


picUrl : String
picUrl =
    "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats"


picDecoder : Decode.Decoder String
picDecoder =
    Decode.at [ "data", "image_url" ] Decode.string
