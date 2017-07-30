module Parallel exposing (..)

import Html exposing (div, text, button, Html, img)
import Html.Attributes exposing (src, height)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Task
import Debug exposing (log)


main : Program Never Model Msg
main =
    Html.program
        { init = { values = [], partialValues = [] } ! [ getValues ]
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { values : List String
    , partialValues : List String
    }



-- View


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Request ] [ text "add img" ]
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
            model ! [ getValues ]

        Success values ->
            { model | values = model.values ++ values } ! []

        Error msg ->
            model ! []

        PartialSuccess updater ->
            updater model


getValues : Cmd Msg
getValues =
    parallelize requestTasks


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


parseHttpResult : Result Http.Error (List String) -> Msg
parseHttpResult result =
    case result of
        Ok values ->
            Success values

        Err msg ->
            Error msg



-- Parallelize


parallelize : List (Task.Task Http.Error String) -> Cmd Msg
parallelize tasks =
    Cmd.batch
        (List.map
            (Task.attempt
                (\result ->
                    case result of
                        Ok value ->
                            PartialSuccess (\model -> partialSuccessUpdater value model)

                        Err msg ->
                            Error msg
                )
            )
            tasks
        )


partialSuccessUpdater : String -> Model -> ( Model, Cmd Msg )
partialSuccessUpdater value model =
    if (allFinished requests model.partialValues) then
        ( { model | partialValues = [] }, cmdSuccess (model.partialValues ++ [ value ]) )
    else
        ( { model | partialValues = model.partialValues ++ [ value ] }, Cmd.none )


allFinished : List a -> List b -> Bool
allFinished tasks partialValues =
    List.length tasks == List.length partialValues + 1


cmdSuccess : List String -> Cmd Msg
cmdSuccess values =
    Task.perform Success (Task.succeed values)
