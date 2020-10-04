port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, text, textarea)
import Html.Attributes exposing (class, classList, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Time exposing (Month(..), Posix, toDay, toMonth, toYear, utc)
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
    | RemoveLastHabitEntry
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

        RemoveLastHabitEntry ->
            let
                nextLog =
                    case List.tail model.habitLog of
                        Just v ->
                            v

                        Nothing ->
                            []
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
    { title = "Building Habits"
    , body = [ pageContent model ]
    }


pageContent : Model -> Html Msg
pageContent model =
    let
        daysCompleted =
            timesHabitWasCompleted model.habit model.habitLog

        daysCompletedText =
            String.fromInt daysCompleted ++ " / 21"

        habitCompleteButton =
            if completedToday model then
                div []
                    [ div
                        [ class <| primaryButton ++ " text-gray-800"
                        , onClick RemoveLastHabitEntry
                        ]
                        [ Icons.undo purple 36 ]
                    ]

            else
                button
                    [ class <| primaryButton ++ " bg-purple-700 hover:bg-purple-800", onClick CompleteHabit ]
                    [ Icons.check white 36 ]
    in
    div []
        [ div [ class "grid h-screen px-4 py-12" ]
            [ div [ class "flex flex-col justify-end" ]
                [ h1
                    [ classList
                        [ ( header ++ " overflow-y-scroll max-h-64 mb-10 break-anywhere", True )
                        , ( "line-through", completedToday model )
                        ]
                    , onClick ToggleEditHabit
                    ]
                    [ text model.habit ]
                , div [ class daysText ] [ text daysCompletedText ]
                ]
            , div [ class "flex items-end justify-center" ] [ habitCompleteButton ]
            ]
        , editHabitModal model
        , completedTodayText model
        ]


completedTodayText : Model -> Html Msg
completedTodayText model =
    if completedToday model then
        div
            [ class "absolute top-0 mt-8 w-full text-gray-600 text-center"
            ]
            [ text "Come back tomorrow!" ]

    else
        text ""


editHabitModal : Model -> Html Msg
editHabitModal model =
    div
        [ classList
            [ ( "absolute z-20 left-0 top-0 bottom-0 right-0 bg-white", True )
            , ( "hidden", not model.editing )
            ]
        ]
        [ textarea
            [ value model.habit
            , onInput UpdateHabit
            , classList [ ( header, True ), ( "py-8 px-4 resize-none h-screen", True ) ]
            ]
            []
        , button
            [ class "absolute right-0 bottom-0 mr-8 mb-8 p-4 rounded-full bg-purple-700"
            , onClick ToggleEditHabit
            ]
            [ div [ class "mx-auto w-min-c" ] [ Icons.save white 36 ] ]
        ]



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


gray : String
gray =
    "#aeaeae"


white : String
white =
    "#ffffff"


purple : String
purple =
    "#6b46c1"


header : String
header =
    "font-display font-bold leading-tight text-3xl text-gray-900"


daysText : String
daysText =
    "font-mono bold text-lg"


primaryButton : String
primaryButton =
    "text-white font-bold p-4 rounded-full"
