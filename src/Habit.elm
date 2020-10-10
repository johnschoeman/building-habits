module Habit exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Time exposing (Month(..), Posix, toDay, toMonth, toYear, utc)


type alias Habit =
    String


type alias HabitEntry =
    { habit : Habit
    , date : Posix
    }


type alias HabitLog =
    List HabitEntry



---- Encoding / Decoding ----


encodeHabitLog : HabitLog -> Encode.Value
encodeHabitLog habitLog =
    Encode.list encodeHabitEntry habitLog


encodeHabitEntry : HabitEntry -> Encode.Value
encodeHabitEntry entry =
    Encode.object
        [ ( "habit", Encode.string entry.habit )
        , ( "date", Encode.int (Time.posixToMillis entry.date) )
        ]


decodeHabitLog : Decode.Value -> HabitLog
decodeHabitLog value =
    case Decode.decodeValue (Decode.list decodeHabitEntry) value of
        Ok history ->
            history

        Err _ ->
            []


decodeHabitEntry : Decode.Decoder HabitEntry
decodeHabitEntry =
    Decode.map2 HabitEntry
        (Decode.field "habit" Decode.string)
        (Decode.field "date" Decode.int |> Decode.andThen decodePosix)


decodePosix : Int -> Decode.Decoder Posix
decodePosix v =
    Decode.succeed <| Time.millisToPosix v



---- Analytics ----


format : Habit -> Habit
format habit =
    String.trim <| String.toLower habit


isEqual : Habit -> Habit -> Bool
isEqual habitA habitB =
    format habitA == format habitB


completedToday : Time.Zone -> Habit -> Posix -> HabitLog -> Bool
completedToday zone habit now habitLog =
    let
        logCompletedToday log =
            isEqual habit log.habit && isSameDay zone log.date now
    in
    List.any logCompletedToday habitLog


isSameDay : Time.Zone -> Posix -> Posix -> Bool
isSameDay zone now time =
    (Time.toYear zone now == Time.toYear zone time)
        && (Time.toMonth zone now == Time.toMonth zone time)
        && (Time.toDay zone now == Time.toDay zone time)


timesHabitWasCompleted : Habit -> HabitLog -> Int
timesHabitWasCompleted habit habitLog =
    let
        entryMatchesCurrentHabit habitEntry acc =
            if isEqual habitEntry.habit habit then
                acc + 1

            else
                acc
    in
    List.foldl entryMatchesCurrentHabit 0 habitLog
