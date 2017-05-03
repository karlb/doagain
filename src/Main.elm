port module Todo exposing (..)

import Html
import Json.Encode
import Json.Decode
import Time exposing (Time)
import Platform.Cmd
import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Helper exposing (..)


main : Program (Maybe Json.Decode.Value) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateSortAndSave
        , subscriptions = subscriptions
        }



-- LOAD AND SAVE


port setStorage : Json.Encode.Value -> Cmd msg


updateSortAndSave : Msg -> Model -> ( Model, Cmd Msg )
updateSortAndSave msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( { newModel | entries = sortEntries newModel.entries newModel.now }
        , Cmd.batch [ saveModel newModel msg, cmds ]
        )


saveModel : Model -> Msg -> Cmd Msg
saveModel model msg =
    case msg of
        -- no need to save model when only the current time has changed
        Tick _ ->
            Platform.Cmd.none

        _ ->
            setStorage (modelEncoder model)


init : Maybe Json.Decode.Value -> ( Model, Cmd Msg )
init savedModel =
    let
        model =
            case savedModel of
                Nothing ->
                    emptyModel

                Just jsonString ->
                    case Json.Decode.decodeValue modelDecoder jsonString of
                        Ok value ->
                            value

                        Err error ->
                            Debug.crash "could not load model"
    in
        update (GetTimeAndThen Tick) model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Tick
        , Time.every Time.hour NextDay
        ]
