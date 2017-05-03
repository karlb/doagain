module Helper exposing (..)

import Date
import Date.Extra
import Time exposing (Time)
import List.Extra


fmtTime : Time -> String
fmtTime time =
    Date.Extra.toFormattedString "yyyy-MM-dd" (Date.fromTime time)


fmtTimeDiff : Time -> Time -> String
fmtTimeDiff t1 t2 =
    let
        day_start t =
            Date.fromTime t |> Date.Extra.floor Date.Extra.Day

        days =
            Date.Extra.diff Date.Extra.Day
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
            (abs days |> toString) ++ " days ago"
        else
            "in " ++ (days |> toString) ++ " days"


avgDiff : List Float -> Maybe Float
avgDiff l =
    let
        diff_l =
            diff l
    in
        if diff_l == [] then
            Nothing
        else
            Just
                ((List.sum diff_l) / toFloat (List.length diff_l))


diff : List Float -> List Float
diff l =
    case l of
        a :: b :: rest ->
            (a - b) :: (diff (b :: rest))

        _ ->
            []
