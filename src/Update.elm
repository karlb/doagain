module Update exposing (..)

import Browser.Dom as Dom
import Date
import Helper exposing (..)
import Json.Decode
import Json.Encode
import List
import List.Extra
import Model exposing (..)
import Task
import Time


type Msg
    = NoOp
    | UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | EditingDate Int Int String
    | UpdateDate Int String
    | FinishDateEdit Int Int String
    | AddDate Entry Time.Posix
    | AbortEdit
    | Add
    | Delete Int
    | Check Int Bool Time.Posix
    | ChangeVisibility (Maybe String)
    | GetTimeAndThen (Time.Posix -> Msg)
    | Tick Time.Posix
    | NextDay Time.Posix
    | AddTag Int
    | RemoveTag Int String
    | UpdateTag Int String
    | FinishTagEdit Int String
    | NewState Json.Encode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        Add ->
            ( { model
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries

                    else
                        model.entries ++ [ newEntry model.field model.uid model.visibility ]
              }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | editing = isEditing }

                    else
                        t

                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Task.attempt (\_ -> NoOp) focus
            )

        EditingDate id dateIndex startText ->
            ( { model | edit = EditDate id dateIndex startText }
            , Task.attempt (\_ -> NoOp) (Dom.focus "focus-this")
            )

        UpdateDate id dateString ->
            ( { model
                | edit =
                    case model.edit of
                        EditDate id2 dateIndex _ ->
                            EditDate id2 dateIndex dateString

                        _ ->
                            NoEdit
              }
            , Cmd.none
            )

        FinishDateEdit id dateIndex text ->
            if text == "" then
                let
                    updateEntry e =
                        { e | doneAt = List.Extra.removeAt dateIndex e.doneAt }
                in
                ( { model
                    | edit = NoEdit
                    , entries =
                        List.Extra.updateIf
                            (\e -> e.id == id)
                            updateEntry
                            model.entries
                  }
                , Cmd.none
                )

            else
                case posixFromIso text of
                    Ok newDate ->
                        let
                            updateEntry e =
                                let
                                    newDoneAt =
                                        List.Extra.setAt
                                            dateIndex
                                            newDate
                                            e.doneAt
                                in
                                { e | doneAt = newDoneAt |> sortTime |> List.reverse }
                        in
                        ( { model
                            | edit = NoEdit
                            , entries =
                                List.Extra.updateIf
                                    (\e -> e.id == id)
                                    updateEntry
                                    model.entries
                          }
                        , Cmd.none
                        )

                    Err _ ->
                        ( model
                        , Cmd.none
                        )

        AddDate todo now ->
            ( { model
                | edit = EditDate todo.id (List.length todo.doneAt) (fmtTime now)
                , entries =
                    List.Extra.updateIf
                        (\e -> e == todo)
                        (\e -> { e | doneAt = List.append e.doneAt [ now ] })
                        model.entries
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus "focus-this")
            )

        AbortEdit ->
            ( { model | edit = NoEdit }
            , Cmd.none
            )

        UpdateEntry id task ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = task }

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        Delete id ->
            ( { model | entries = List.filter (\t -> t.id /= id) model.entries }
            , Cmd.none
            )

        Check id isCompleted time ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted }

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )

        GetTimeAndThen successHandler ->
            ( model, Task.perform successHandler Time.now )

        Tick time ->
            ( { model | now = time }
            , Cmd.none
            )

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
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        AddTag uint ->
            ( { model | edit = EditTag uint "" }
            , Task.attempt (\_ -> NoOp) (Dom.focus "focus-this")
            )

        RemoveTag uid tag ->
            ( { model
                | entries =
                    List.Extra.updateIf
                        (\e -> e.id == uid)
                        (\e -> { e | tags = List.filter (\t -> t /= tag) e.tags })
                        model.entries
              }
            , Cmd.none
            )

        UpdateTag uid text ->
            ( { model | edit = EditTag uid text }
            , Cmd.none
            )

        FinishTagEdit uid text ->
            ( { model
                | edit = NoEdit
                , entries =
                    List.Extra.updateIf
                        (\e -> e.id == uid)
                        (\e -> { e | tags = List.append e.tags [ text ] })
                        model.entries
              }
            , Cmd.none
            )

        NewState newModelJson ->
            case Json.Decode.decodeValue modelDecoder newModelJson of
                Ok newModel ->
                    ( newModel
                    , Cmd.none
                    )

                Err error ->
                    Debug.todo "could not parse new model"
