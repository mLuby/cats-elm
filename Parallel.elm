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



-- Update


type Msg
    = Request
    | Success (List String) -- List a
    | PartialSuccess (Model -> ( Model, Cmd Msg ))
    | Error Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Request ->
            model ! [ getValues ]

        Success values ->
            { model | values = model.values ++ values } ! []

        PartialSuccess updater ->
            updater model

        Error msg ->
            model ! []



-- getValues : String -> Cmd Msg
-- getValues searchTerm =
--     Task.attempt parseHttpResult (Task.sequence requests)
-- getValues searchTerm =
--     Task.attempt parseHttpResult (request searchTerm)


getValues : Cmd Msg
getValues =
    Cmd.batch
        (List.map
            (\requestTask ->
                Task.attempt
                    (\result ->
                        case result of
                            Ok value ->
                                PartialSuccess (\model -> partialSuccessUpdater value model)

                            Err msg ->
                                Error msg
                    )
                    requestTask
            )
            requests
        )


partialSuccessUpdater : String -> Model -> ( Model, Cmd Msg )
partialSuccessUpdater value model =
    if ((List.length requests) - 1) == (List.length model.partialValues) then
        log "foo" ( { model | partialValues = [] }, cmdSuccess (model.partialValues ++ [ value ]) )
    else
        log "bar" ( { model | partialValues = model.partialValues ++ [ value ] }, Cmd.none )


cmdSuccess : List String -> Cmd Msg
cmdSuccess values =
    Task.perform Success (Task.succeed values)


requests : List (Task.Task Http.Error String)
requests =
    List.map request [ "explosion", "rainbow", "ocean" ]



-- merge = first => second => third => fold (\(index,value) -> "order to index")[first,second,third]
-- all([f, f, f]) -> (1,"b") (0,"a") (2,"c") -> ["a", "b", "c"]


request : String -> Task.Task Http.Error String
request searchTerm =
    Http.toTask
        (Http.get
            ("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ searchTerm)
            (Decode.at [ "data", "image_url" ] Decode.string)
        )


parseHttpResult : Result Http.Error (List String) -> Msg
parseHttpResult result =
    case result of
        Ok values ->
            Success values

        Err msg ->
            Error msg



-- need external-reference closure or shareable partial function application
-- need to know results taskIndex tasksLength
-- if Error then Task.fail result
-- if Ok then add value to results by task index
-- if length results == tasksLength then Task.succeed results
-- if Error then Task.fail result
-- if Ok then add value to results by task index
-- if length results == tasksLength then Task.succeed results
-- when given tasks, create func for them to call on completion to see if done
-- return task that'll
-- each task when attempted results in Ok or Err
-- View


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Request ] [ text "add img" ]
        , div [] (List.map (\v -> img [ src v, height 50 ] []) model.values)
        ]
