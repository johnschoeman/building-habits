port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, text, textarea)
import Html.Attributes exposing (autofocus, class, classList, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Time exposing (Month(..), Posix, toDay, toHour, toMinute, toMonth, toYear, utc)
import Url



---- MODEL ----


type alias Habit =
    String


type alias HabitEntry =
    { habit : Habit
    , date : Posix
    }


type alias HabitLog =
    List HabitEntry


type alias Model =
    { habit : Habit
    , habitLog : HabitLog
    , editing : Bool
    , now : Posix
    }


type alias Flags =
    { habit : String
    , habitLog : Decode.Value
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        habitLog =
            decodeHabitLog flags.habitLog
    in
    ( { habit = flags.habit
      , habitLog = habitLog
      , editing = False
      , now = Time.millisToPosix 0
      }
    , Task.perform Now Time.now
    )



---- UPDATE ----


type Msg
    = CompleteHabit
    | UpdateHabit Habit
    | ToggleEditHabit
    | LogHabit Posix
    | ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | Now Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompleteHabit ->
            ( model, Task.perform LogHabit Time.now )

        LogHabit now ->
            let
                habitEntry =
                    { habit = model.habit, date = now }

                nextLog =
                    habitEntry :: model.habitLog
            in
            ( { model | habitLog = nextLog }
            , saveHabitLog nextLog
            )

        UpdateHabit habit ->
            ( { model | habit = habit }
            , saveHabit habit
            )

        ToggleEditHabit ->
            ( { model | editing = not model.editing }, Cmd.none )

        ChangedUrl _ ->
            ( model, Cmd.none )

        ClickedLink _ ->
            ( model, Cmd.none )

        Now now ->
            ( { model | now = now }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


port saveHabitLocally : Encode.Value -> Cmd msg


saveHabit : Habit -> Cmd Msg
saveHabit habit =
    saveHabitLocally <| Encode.string habit


port saveHabitLogLocally : Encode.Value -> Cmd msg


saveHabitLog : HabitLog -> Cmd Msg
saveHabitLog habitLog =
    saveHabitLogLocally <| encodeHabitLog habitLog


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


completedToday : Model -> Bool
completedToday { habit, habitLog, now } =
    let
        logCompletedToday log =
            log.habit == habit && isSameDay log.date now
    in
    List.any logCompletedToday habitLog


isSameDay : Posix -> Posix -> Bool
isSameDay now time =
    (Time.toYear utc now == Time.toYear utc time)
        && (Time.toMonth utc now == Time.toMonth utc time)
        && (Time.toDay utc now == Time.toDay utc time)


timesHabitWasCompleted : Habit -> HabitLog -> Int
timesHabitWasCompleted habit habitLog =
    let
        isSameHabit habitEntry acc =
            if habitEntry.habit == habit then
                acc + 1

            else
                acc
    in
    List.foldl isSameHabit 0 habitLog



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Building Habigs"
    , body = [ pageContent model ]
    }


pageContent : Model -> Html Msg
pageContent model =
    let
        daysCompleted =
            timesHabitWasCompleted model.habit model.habitLog

        daysCompletedText =
            String.fromInt daysCompleted ++ " of 21 days"

        habitCompleteButton =
            if completedToday model then
                text "✓"

            else
                button [ class primaryButton, onClick CompleteHabit ] [ text "✓" ]

        editButtonText =
            if model.editing then
                "Done"

            else
                "Edit"
    in
    div [ class "flex flex-col items-center h-screen" ]
        [ div [ class "flex flex-4 flex-col justify-center align-left w-full p-4" ]
            [ div [ class "flex flex-row justify-between items-end" ]
                [ habitInput model
                , button [ onClick ToggleEditHabit ] [ text editButtonText ]
                ]
            , div [ class <| secondary ++ " h-2" ] [ text daysCompletedText ]
            ]
        , div [ class "flex flex-1 items-center justify-center w-full" ]
            [ habitCompleteButton
            ]
        ]


habitInput : Model -> Html Msg
habitInput model =
    if model.editing then
        textarea
            [ autofocus True
            , value model.habit
            , onInput UpdateHabit
            , classList [ ( header, True ), ( "py-1", True ) ]
            ]
            []

    else
        h1 [ class header ] [ text model.habit ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }



---- STYLES ----


header : String
header =
    "font-display font-bold leading-tight text-4xl text-gray-900"


secondary : String
secondary =
    "font-body text-gray-700 text-xs"


primaryButton : String
primaryButton =
    "bg-indigo-500 hover:bg-indigo-700 text-white font-bold py-4 px-8 rounded-full"
