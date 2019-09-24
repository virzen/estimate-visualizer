module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Task =
    { name : String
    , optimistic : Int
    , pessimistic : Int
    }


type alias Model =
    { tasks : List Task
    , name : Maybe String
    , optimistic : Maybe Int
    , pessimistic : Maybe Int
    }


init =
    { tasks = []
    , name = Nothing
    , optimistic = Nothing
    , pessimistic = Nothing
    }


type Msg
    = Save
    | TaskNameChange String
    | TaskOptimisticChange String
    | TaskPessimisticChange String


update : Msg -> Model -> Model
update msg model =
    case msg of
        TaskNameChange name ->
            { model | name = Just name }

        TaskPessimisticChange pessimistic ->
            case String.toInt pessimistic of
                Just number ->
                    { model | pessimistic = Just number }

                Nothing ->
                    model

        TaskOptimisticChange optimistic ->
            case String.toInt optimistic of
                Just number ->
                    { model | optimistic = Just number }

                Nothing ->
                    model

        Save ->
            case ( model.name, model.optimistic, model.pessimistic ) of
                ( Just name, Just optimistic, Just pessimistic ) ->
                    let
                        newTask =
                            Task name optimistic pessimistic

                        newTasks =
                            List.append model.tasks [ newTask ]
                    in
                    { model | tasks = newTasks }

                _ ->
                    model


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map viewTask model.tasks)
        , text "New task"
        , input [ placeholder "Name", onInput TaskNameChange ] []
        , input [ placeholder "Optimistic estimate", type_ "number", onInput TaskOptimisticChange ] []
        , input [ placeholder "Pessimistic estimate", type_ "number", onInput TaskPessimisticChange ] []
        , button [ onClick Save ] [ text "Save task" ]
        ]


viewTask task =
    div []
        [ text "Task name: "
        , text task.name
        , text ", Optimistic estimate: "
        , task.optimistic |> String.fromInt |> text
        , text ", Pessimistic estimate: "
        , task.pessimistic |> String.fromInt |> text
        ]
