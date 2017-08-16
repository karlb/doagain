module View exposing (view)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode
import Task
import Time exposing (Time)
import Date
import List.Extra exposing (find)
import Model exposing (..)
import Update exposing (..)
import Helper exposing (..)


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style [ ( "visibility", "hidden" ) ]
        ]
        [ section
            [ class "todoapp" ]
            [ lazy viewInput model.field
            , lazy viewEntries model
            , lazy2 viewControls model.visibility model.entries
            ]
        , infoFooter
        ]


viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 [ onClick (GetTimeAndThen (\time -> NextDay time)) ] [ text "Do Again" ]
        , input
            [ class "new-todo"
            , placeholder "What do you want to do again?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not ENTER"
    in
        on "keydown" (Json.Decode.andThen isEnter keyCode)



-- VIEW ALL ENTRIES


viewEntries : Model -> Html Msg
viewEntries model =
    let
        isVisible todo =
            case model.visibility of
                Just tag ->
                    List.member tag todo.tags

                Nothing ->
                    True

        cssVisibility =
            if List.isEmpty model.entries then
                "hidden"
            else
                "visible"
    in
        section
            [ class "main"
            , style [ ( "visibility", cssVisibility ) ]
            ]
            [ Keyed.ul [ class "todo-list" ] <|
                List.map
                    (\e -> viewKeyedEntry e model.edit model.now)
                    (List.filter isVisible model.entries)
            ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Entry -> Edit -> Time -> ( String, Html Msg )
viewKeyedEntry todo edit now =
    ( toString todo.id, lazy3 viewEntry todo edit now )


viewSingleDone : Time -> Int -> Int -> Edit -> Html Msg
viewSingleDone done uid index edit =
    let
        defaultResult =
            span
                [ onDoubleClick
                    (EditingDate uid
                        index
                        (done |> fmtTime)
                    )
                ]
                [ text (fmtTime done) ]
    in
        case edit of
            EditDate todoId dateIndex editText ->
                if uid == todoId && index == dateIndex then
                    input
                        [ value editText
                        , id "focus-this"
                        , onInput (UpdateDate todoId)
                        , onBlur AbortEdit
                        , onEnter (FinishDateEdit uid dateIndex editText)
                        ]
                        []
                else
                    defaultResult

            _ ->
                defaultResult


viewDone : Entry -> Edit -> List (Html Msg)
viewDone todo edit =
    if (List.length todo.doneAt) == 0 then
        []
    else
        [ div
            [ class "done" ]
            ((if todo.doneAt == [] then
                [ text "Not done, yet" ]
              else
                text "Done on "
                    :: List.intersperse
                        (text ", ")
                        (List.indexedMap
                            (\i d -> viewSingleDone d todo.id i edit)
                            todo.doneAt
                        )
             )
                ++ [ span
                        [ class "plus"
                        , onClick (GetTimeAndThen (AddDate todo))
                        ]
                        [ text "+" ]
                   ]
            )
        ]


viewTags : Entry -> Edit -> Html Msg
viewTags todo edit =
    let
        editString =
            case edit of
                EditTag uid text ->
                    if todo.id == uid then
                        Just text
                    else
                        Nothing

                _ ->
                    Nothing
    in
        div
            [ class "tags" ]
            ((if List.isEmpty todo.tags then
                [ div [ class "tag" ] [ text "No tags" ] ]
              else
                List.map
                    (\t ->
                        div
                            [ class "tag" ]
                            [ text t
                            , span
                                [ class "remove"
                                , onClick (RemoveTag todo.id t)
                                ]
                                [ text "×" ]
                            ]
                    )
                    todo.tags
             )
                ++ [ case editString of
                        Just text ->
                            input
                                [ id "focus-this"
                                , value text
                                , onInput (UpdateTag todo.id)
                                , onBlur (AbortEdit)
                                , onEnter (FinishTagEdit todo.id text)
                                ]
                                []

                        Nothing ->
                            span
                                [ onClick (AddTag todo.id) ]
                                [ text "+" ]
                   ]
            )


viewEntry : Entry -> Edit -> Time -> Html Msg
viewEntry todo edit now =
    li
        [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
        [ div
            [ class "view" ]
            (List.concat
                [ [ input
                        [ class "toggle"
                        , type_ "checkbox"
                        , checked todo.completed
                        , onClick (GetTimeAndThen (\time -> Check todo.id (not todo.completed) time))
                        ]
                        []
                  , label
                        [ onDoubleClick (EditingEntry todo.id True) ]
                        [ text (todo.description)
                        , span
                            [ class "due" ]
                            [ text
                                (case due todo of
                                    Nothing ->
                                        ""

                                    Just dueTime ->
                                        " - due " ++ (fmtTimeDiff now dueTime)
                                )
                            ]
                        ]
                  , button
                        [ class "destroy"
                        , onClick (Delete todo.id)
                        ]
                        []
                  ]
                , (viewDone todo edit)
                , [ viewTags todo edit ]
                ]
            )
        , input
            [ class "edit"
            , value todo.description
            , name "title"
            , id ("todo-" ++ toString todo.id)
            , onInput (UpdateEntry todo.id)
            , onBlur (EditingEntry todo.id False)
            , onEnter (EditingEntry todo.id False)
            ]
            []
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : Maybe String -> List Entry -> Html Msg
viewControls visibility entries =
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ lazy2 viewControlsFilters visibility entries ]


viewControlsFilters : Maybe String -> List Entry -> Html Msg
viewControlsFilters visibility entries =
    ul
        [ class "filters" ]
        (List.intersperse
            (text " ")
            (List.map (\e -> e.tags) entries
                |> List.concat
                |> List.Extra.unique
                |> List.map (\tag -> Just tag)
                |> (::) Nothing
                |> List.map (\tag -> visibilitySwap tag visibility)
            )
        )


visibilitySwap : Maybe String -> Maybe String -> Html Msg
visibilitySwap visibility actualVisibility =
    li
        [ onClick (ChangeVisibility (visibility)) ]
        [ a [ classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text
                (case visibility of
                    Just vis ->
                        vis

                    Nothing ->
                        "All"
                )
            ]
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
