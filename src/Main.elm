port module Main exposing (..)

import Browser
import Helper exposing (..)
import Html
import Json.Decode
import Json.Encode
import Model exposing (..)
import Platform.Cmd
import Time
import Update exposing (..)
import View exposing (..)


main : Program (Maybe Json.Decode.Value) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "DoAgain", body = [ view model ] }
        , update = updateSortAndSave
        , subscriptions = subscriptions
        }



-- LOAD AND SAVE


port setStorage : Json.Encode.Value -> Cmd msg


port setState : (Json.Encode.Value -> msg) -> Sub msg


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
                            Debug.todo "could not load model"
    in
    update (GetTimeAndThen Tick) model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick -- Every second
        , Time.every (1000 * 60 * 60) NextDay -- Every hour
        , setState NewState
        ]
