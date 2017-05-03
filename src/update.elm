module Update exposing (..)

import Dom
import Task
import Time exposing (Time)
import Date
import List
import List.Extra exposing (find)
import Model exposing (..)
import Helper exposing (..)


type Msg
    = NoOp
    | UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | EditingDate Int Int String
    | UpdateDate Int String
    | FinishDateEdit Int Int String
    | AddDate Entry Time
    | AbortEdit
    | Add
    | Delete Int
    | Check Int Bool Time
    | ChangeVisibility (Maybe String)
    | GetTimeAndThen (Time -> Msg)
    | Tick Time
    | NextDay Time
    | AddTag Int
    | UpdateTag Int String
    | FinishTagEdit Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Add ->
            { model
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries
                    else
                        model.entries ++ [ newEntry model.field model.uid model.visibility ]
            }
                ! []

        UpdateField str ->
            { model | field = str }
                ! []

        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | editing = isEditing }
                    else
                        t

                focus =
                    Dom.focus ("todo-" ++ toString id)
            in
                { model | entries = List.map updateEntry model.entries }
                    ! [ Task.attempt (\_ -> NoOp) focus ]

        EditingDate id dateIndex startText ->
            { model | edit = EditDate id dateIndex startText }
                ! [ Task.attempt (\_ -> NoOp) (Dom.focus ("focus-this")) ]

        UpdateDate id dateString ->
            { model
                | edit =
                    case model.edit of
                        EditDate id dateIndex _ ->
                            EditDate id dateIndex dateString

                        _ ->
                            NoEdit
            }
                ! []

        FinishDateEdit id dateIndex text ->
            if text == "" then
                let
                    updateEntry e =
                        { e | doneAt = List.Extra.removeAt dateIndex e.doneAt }
                in
                    { model
                        | edit = NoEdit
                        , entries =
                            List.Extra.updateIf
                                (\e -> e.id == id)
                                updateEntry
                                model.entries
                    }
                        ! []
            else
                case Date.fromString text of
                    Ok newDate ->
                        let
                            updateEntry e =
                                case
                                    (List.Extra.setAt
                                        dateIndex
                                        (Date.toTime newDate)
                                        e.doneAt
                                    )
                                of
                                    Just newDoneAt ->
                                        { e | doneAt = newDoneAt |> List.sort |> List.reverse }

                                    Nothing ->
                                        Debug.crash "bad dateIndex"
                        in
                            { model
                                | edit = NoEdit
                                , entries =
                                    List.Extra.updateIf
                                        (\e -> e.id == id)
                                        updateEntry
                                        model.entries
                            }
                                ! []

                    Err _ ->
                        model ! []

        AddDate todo now ->
            { model
                | edit = EditDate todo.id (List.length todo.doneAt) (fmtTime now)
                , entries =
                    List.Extra.updateIf
                        (\e -> e == todo)
                        (\e -> { e | doneAt = List.append e.doneAt [ now ] })
                        model.entries
            }
                ! [ Task.attempt (\_ -> NoOp) (Dom.focus ("focus-this")) ]

        AbortEdit ->
            { model | edit = NoEdit } ! []

        UpdateEntry id task ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = task }
                    else
                        t
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        Delete id ->
            { model | entries = List.filter (\t -> t.id /= id) model.entries }
                ! []

        Check id isCompleted time ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted }
                    else
                        t
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        ChangeVisibility visibility ->
            { model | visibility = visibility }
                ! []

        GetTimeAndThen successHandler ->
            ( model, (Task.perform successHandler Time.now) )

        Tick time ->
            { model | now = time } ! []

        NextDay time ->
            -- uncheck all
            let
                updateEntry t =
                    { t
                        | completed = False
                        , doneAt =
                            case t.completed of
                                True ->
                                    time :: t.doneAt

                                False ->
                                    t.doneAt
                    }
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        AddTag uint ->
            { model | edit = EditTag uint "" }
                ! [ Task.attempt (\_ -> NoOp) (Dom.focus "focus-this") ]

        UpdateTag uid text ->
            { model | edit = EditTag uid text }
                ! []

        FinishTagEdit uid text ->
            { model
                | edit = NoEdit
                , entries =
                    List.Extra.updateIf
                        (\e -> e.id == uid)
                        (\e -> { e | tags = List.append e.tags [ text ] })
                        model.entries
            }
                ! []
