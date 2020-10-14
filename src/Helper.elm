module Helper exposing (..)

import Date exposing (Interval(..), Unit(..))
import Time
import Time.Extra


fmtTime : Time.Posix -> String
fmtTime time =
        -- TODO: use Time.here instead of UTC
        Date.toIsoString (Date.fromPosix Time.utc time)


fmtTimeDiff : Time.Posix -> Time.Posix -> String
fmtTimeDiff t1 t2 =
    let
        day_start t =
            Date.floor Day (Date.fromPosix Time.utc t)

        days =
            Date.diff Days
                (day_start t1)
                (day_start t2)
    in
    if days == 0 then
        "today"

    else if days == 1 then
        "tomorrow"

    else if days == -1 then
        "yesterday"

    else if days < 0 then
        (abs days |> String.fromInt) ++ " days ago"

    else
        "in " ++ (days |> String.fromInt) ++ " days"


avgDiff : List Int -> Maybe Int
avgDiff l =
    let
        diff_l =
            diff l
    in
    if diff_l == [] then
        Nothing

    else
        Just
            (List.sum diff_l // List.length diff_l)


diff : List Int -> List Int
diff l =
    case l of
        a :: b :: rest ->
            (a - b) :: diff (b :: rest)

        _ ->
            []


sortTime : List Time.Posix -> List Time.Posix
sortTime l =
    List.sortWith (\a b -> compare (Time.posixToMillis a) (Time.posixToMillis b)) l


posixFromIso : String -> Result String Time.Posix
posixFromIso iso_date =
    case Date.fromIsoString iso_date of
        Ok date ->
            Ok
                -- TODO: use Time.here instead of Time.utc
                (Time.Extra.Parts (Date.year date) (Date.month date) (Date.day date) 0 0 0 0
                    |> Time.Extra.partsToPosix Time.utc
                )

        Err e ->
            Err e
