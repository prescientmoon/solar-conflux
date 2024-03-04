module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- HELPER TYPES


type alias Todo =
    { done : Bool
    , text : String
    , id : Int
    }



---- MODEL ----


type alias Model =
    { todos : List Todo
    , lastId : Int
    , nextTodoText : String
    }


init : Model
init =
    Model [] 0 ""



---- UPDATE ----


type Msg
    = AddTodo
    | DeleteTodo Int
    | ToggleTodoCompletion Int
    | SetNextTodoText String


update : Msg -> Model -> Model
update message model =
    case message of
        AddTodo ->
            if model.nextTodoText == "" then
                model

            else
                let
                    id =
                        model.lastId + 1

                    text =
                        model.nextTodoText
                in
                { model | lastId = id, todos = Todo False text id :: model.todos }

        DeleteTodo id ->
            { model | todos = List.filter ((/=) id << .id) model.todos }

        ToggleTodoCompletion id ->
            { model
                | todos =
                    List.map
                        (\todo ->
                            if todo.id == id then
                                { todo | done = not todo.done }

                            else
                                todo
                        )
                        model.todos
            }

        SetNextTodoText text ->
            { model | nextTodoText = text }



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ header model
        , div [ class "todos" ] <|
            List.map
                todoView
                model.todos
        ]



-- HEADER


header : Model -> Html Msg
header _ =
    div [ class "header" ]
        [ button
            [ class "btn"
            , onClick AddTodo
            ]
            [ text "add todo" ]
        , input
            [ placeholder "todo text"
            , onInput SetNextTodoText
            , class "todoTextInput"
            ]
            []
        ]



-- _TODO VIEW


todoClasses : Todo -> String
todoClasses todo =
    "todo"
        ++ (if todo.done then
                " completed"

            else
                ""
           )


todoView : Todo -> Html Msg
todoView todo =
    div [ class <| todoClasses todo ]
        [ div [ class "todoCompleted" ]
            [ input
                [ checked todo.done
                , type_ "checkbox"
                , class "todoCheckbox"
                , onCheck <| \_ -> ToggleTodoCompletion todo.id
                ]
                []
            ]
        , div [ class "todoText" ] [ text todo.text ]
        , button
            [ class "btn"
            , onClick <| DeleteTodo todo.id
            ]
            [ text "Delete todo" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }
