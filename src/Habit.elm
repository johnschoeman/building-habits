module Habit exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Time exposing (Month(..), Posix, toDay, toMonth, toYear, utc)


type alias Habit =
    { id : String
    , title : String
    }


type alias EventV1 =
    { habit : String
    , date : Posix
    }


type alias HabitCompletionEvent =
    { habit : Habit
    , date : Posix
    }


type alias HabitLog =
    List HabitCompletionEvent



---- Encoding / Decoding ----


encodeHabitLog : HabitLog -> Encode.Value
encodeHabitLog habitLog =
    Encode.list encodeHabitCompletionEvent habitLog


encodeHabitCompletionEvent : HabitCompletionEvent -> Encode.Value
encodeHabitCompletionEvent event =
    Encode.object
        [ ( "habit", encodeHabit event.habit )
        , ( "date", Encode.int (Time.posixToMillis event.date) )
        ]


encodeHabit : Habit -> Encode.Value
encodeHabit habit =
    Encode.object
        [ ( "id", Encode.string habit.id )
        , ( "title", Encode.string habit.title )
        ]


decodeHabit : Decode.Value -> Habit
decodeHabit value =
    case Decode.decodeValue habitDecoder value of
        Ok habit ->
            habit

        Err _ ->
            { id = "0", title = "Pick a habit" }


decodeHabitLog : Decode.Value -> HabitLog
decodeHabitLog value =
    case Decode.decodeValue (Decode.list decodeHabitCompletionEvent) value of
        Ok history ->
            history

        Err _ ->
            []


decodeHabitCompletionEvent : Decode.Decoder HabitCompletionEvent
decodeHabitCompletionEvent =
    Decode.oneOf [ decodeHabitCompletionEventV2, decodeHabitCompletionEventV1 ]


decodeHabitCompletionEventV2 : Decode.Decoder HabitCompletionEvent
decodeHabitCompletionEventV2 =
    Decode.map2 HabitCompletionEvent
        (Decode.field "habit" habitDecoder)
        (Decode.field "date" Decode.int |> Decode.andThen decodePosix)


habitDecoder : Decode.Decoder Habit
habitDecoder =
    Decode.map2 Habit
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)


decodeHabitCompletionEventV1 : Decode.Decoder HabitCompletionEvent
decodeHabitCompletionEventV1 =
    Decode.map2 HabitCompletionEvent
        (Decode.field "habit" (Decode.map v1toV2 Decode.string))
        (Decode.field "date" Decode.int |> Decode.andThen decodePosix)


v1toV2 : String -> Habit
v1toV2 habitTitle =
    { id = habitTitle
    , title = habitTitle
    }


decodePosix : Int -> Decode.Decoder Posix
decodePosix v =
    Decode.succeed <| Time.millisToPosix v



---- CRUD ----


addEntry : Habit -> Time.Posix -> HabitLog -> HabitLog
addEntry habit now log =
    { habit = habit, date = now } :: log


undoLastHabit : Habit -> HabitLog -> HabitLog
undoLastHabit habit log =
    let
        firstIdx =
            List.Extra.findIndex (\entry -> isEqual entry.habit habit) log
    in
    case firstIdx of
        Just idx ->
            List.Extra.removeAt idx log

        Nothing ->
            log



---- Analytics ----


toString : Habit -> String
toString habit =
    String.trim <| String.toLower habit.title


isEqual : Habit -> Habit -> Bool
isEqual habitA habitB =
    habitA.id == habitB.id


completedToday : Time.Zone -> Habit -> Posix -> HabitLog -> Bool
completedToday zone habit now habitLog =
    let
        eventCompletedToday event =
            isEqual habit event.habit && isSameDay zone event.date now
    in
    List.any eventCompletedToday habitLog


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
