module Parallel exposing (..)

import Html exposing (div, text, button, Html, img)
import Html.Attributes exposing (src, height)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = { flash = "starting", values = [], partialValues = [] } ! [ getValues ]
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { flash : String
    , values : List String
    , partialValues : List String
    }



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text model.flash ]
        , button [ onClick Request ] [ text "add img" ]
        , div [] (List.map (\v -> img [ src v, height 50 ] []) model.values)
        ]



-- Update


type Msg
    = Request
    | Success (List String)
    | PartialSuccess (Model -> ( Model, Cmd Msg ))
    | Error Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Request ->
            { model | flash = "requesting" } ! [ getValues ]

        Success values ->
            { model | flash = "success", values = model.values ++ values } ! []

        Error msg ->
            { model | flash = errorMapper msg } ! []

        PartialSuccess updater ->
            updater model


getValues : Cmd Msg
getValues =
    parallelize parseHttpResults requestTasks


requestTasks : List (Task.Task Http.Error String)
requestTasks =
    List.map Http.toTask requests


requests : List (Http.Request String)
requests =
    List.map request [ "explosion", "rainbow", "ocean" ]


request : String -> Http.Request String
request searchTerm =
    Http.get
        ("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ searchTerm)
        (Decode.at [ "data", "image_url" ] Decode.string)


errorMapper : Http.Error -> String
errorMapper error =
    "Http Error: "
        ++ case error of
            Http.BadUrl s ->
                "you did not provide a valid URL" ++ s

            Http.Timeout ->
                "it took too long to get a response"

            Http.NetworkError ->
                "the user turned off their wifi, went in a cave, etc"

            Http.BadStatus _ ->
                "got a response back, but the status code indicates failure"

            Http.BadPayload s _ ->
                "got a response back with a nice status code, but the body of the response was something unexpected" ++ s


parseHttpResults : Result Http.Error (List String) -> Msg
parseHttpResults result =
    case result of
        Ok values ->
            Success values

        Err msg ->
            Error msg



-- Parallelize
-- TODO make sure results in same order as corresponding tasks.

parallelize :
    (Result a (List String) -> Msg)
    -> List (Task.Task a String)
    -> Cmd Msg
parallelize taskForker tasks =
    Cmd.batch
        (List.indexedMap
            (\index task ->
                Task.attempt
                    (\result ->
                        case result of
                            Ok value ->
                                PartialSuccess
                                    (\model ->
                                        if (allFinished tasks model.partialValues) then
                                            ( { model | partialValues = [] }, cmdSuccess taskForker (model.partialValues ++ [ value ]) )
                                        else
                                            ( { model | partialValues = model.partialValues ++ [ value ] }, Cmd.none )
                                    )

                            Err msg ->
                                taskForker (Result.Err msg)
                    )
                    task
            )
            tasks
        )


allFinished : List a -> List b -> Bool
allFinished tasks partialValues =
    List.length tasks == List.length partialValues + 1


cmdSuccess : (Result error a -> Msg) -> a -> Cmd Msg
cmdSuccess taskForker values =
    Task.perform (\_ -> taskForker (Result.Ok values)) (Task.succeed values)
