module Cats exposing (..)

import Html exposing (div, h1, button, li, ul, text, Html, span, h2, img)
import Html.Attributes exposing (hidden, src, height)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Task
import List exposing (map, length, range)


-- EDIT: see http://package.elm-lang.org/packages/NoRedInk/elm-task-extra/2.0.0/Task-Extra#parallel
-- Achieves parallelism by creating empty cat then filling it in.
-- stillLoading counter = number of requests,
-- start requests pointing to cat (index?)
-- when each request returns, it fills in part of cat and decrements counter
-- could skip the counter and check vals if you know they must all not be
-- if some requests are optional, they don't have to decrement.
-- pass updater function to update to run against cats. map over cats, find id, fill in value.


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModal, Cmd.batch (List.indexedMap (\id _ -> getCat id) initialModal.cats) )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


numCatsToStart : Int
numCatsToStart =
    2



-- Redux state = Elm model.


type alias Model =
    { flash : String
    , cats : List Cat
    }


initialModal : Model
initialModal =
    { flash = "Initializing…"
    , cats = range 0 (numCatsToStart - 1) |> map (createCat numSubRequests)
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
            ( { model | cats = model.cats ++ [ createCat numSubRequests (nextIndex model.cats) ], flash = "Requesting parallel cat…" }, getCat (nextIndex model.cats) )

        ReceivePartialCat catUpdater ->
            ( { model | cats = List.map catUpdater model.cats }, Cmd.none )

        FetchCatFail error ->
            ( { model | flash = "fetch error" }, Cmd.none )


createCat : Int -> Int -> Cat
createCat requiredRequestCount id =
    { id = id, stillLoading = requiredRequestCount, fact = "", pic = "" }


nextIndex : List a -> Int
nextIndex list =
    list
        |> List.length
        |> (+) 1



-- React render = Elm view.


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Cats" ]
        , h2 [] [ text model.flash ]
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
        [ img [ src cat.pic, height 50 ] []
        , span [] [ text cat.fact ]
        ]



-- HTTP


getCat : Int -> Cmd Msg
getCat catId =
    Cmd.batch (map (\req -> req catId) subRequests)



-- Want to apply the id to each element but Elm has no apply.


numSubRequests : Int
numSubRequests =
    length subRequests


subRequests : List (Int -> Cmd Msg)
subRequests =
    [ getCatFact, getCatPic ]


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
