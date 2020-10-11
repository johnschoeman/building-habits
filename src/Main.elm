port module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Navigation as Nav
import Habit
    exposing
        ( Habit
        , HabitEntry
        , HabitLog
        , addEntry
        , completedToday
        , decodeHabitLog
        , encodeHabitLog
        , timesHabitWasCompleted
        , undoLastHabit
        )
import Html exposing (Html, button, div, h1, text, textarea)
import Html.Attributes exposing (class, classList, id, style, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode
import Styles.Buttons as Buttons
import Styles.Colors as Colors
import Styles.Typography as Typography
import Task
import Time exposing (Month(..), Posix)
import Url



---- MODEL ----


type alias Context =
    { habit : Habit
    , habitLog : HabitLog
    , now : Posix
    , timeZone : Time.Zone
    , viewport : Viewport
    }


buildInitialContext : BootData -> SystemData -> Context
buildInitialContext { flags } { now, zone, viewport } =
    let
        habitLog =
            decodeHabitLog flags.habitLog
    in
    { habit = flags.habit
    , habitLog = habitLog
    , now = now
    , timeZone = zone
    , viewport = viewport
    }


type alias Flags =
    { habit : String
    , habitLog : Decode.Value
    }


type alias BootData =
    { flags : Flags
    }


type alias SystemData =
    { now : Posix
    , zone : Time.Zone
    , viewport : Viewport
    }


type Model
    = Booting BootData
    | App Screen Context


type Screen
    = Habit
    | Log
    | EditHabit


totalDays =
    21


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        getSystemData =
            Task.map3 SystemData
                Time.now
                Time.here
                Browser.Dom.getViewport
    in
    ( Booting { flags = flags }
    , Task.perform GotSystemData getSystemData
    )



---- UPDATE ----


type Msg
    = GotSystemData SystemData
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url
    | ChangedScreen Screen
    | HandleHabitMsg HabitMsg
    | HandleEditHabitsMsg EditHabitMsg
    | HandleLogMsg LogMsg


type HabitMsg
    = HabitChangeScreen Screen
    | CompleteHabit
    | LogHabit Posix
    | RemoveLastHabitEntry
    | HabitNoOp


type EditHabitMsg
    = EditHabitChangeScreen Screen
    | UpdateHabit Habit


type LogMsg
    = LogChangeScreen Screen
    | LogNoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Booting bootData, GotSystemData systemData ) ->
            let
                initialContext =
                    buildInitialContext bootData systemData
            in
            ( App Habit initialContext, Cmd.none )

        ( Booting bootData, _ ) ->
            ( model, Cmd.none )

        ( App _ context, ChangedUrl _ ) ->
            ( model, Cmd.none )

        ( App _ context, ClickedLink _ ) ->
            ( model, Cmd.none )

        ( App _ context, ChangedScreen screen ) ->
            ( App screen context, Cmd.none )

        ( App screen context, subModelMsg ) ->
            handleSubModelMsg screen context subModelMsg


handleSubModelMsg : Screen -> Context -> Msg -> ( Model, Cmd Msg )
handleSubModelMsg screen context msg =
    case ( screen, msg ) of
        ( Habit, HandleHabitMsg subMsg ) ->
            let
                ( nextScreen, nextContext, nextSubMsg ) =
                    updateHabitScreen subMsg context
            in
            ( App nextScreen nextContext, Cmd.map HandleHabitMsg nextSubMsg )

        ( Habit, _ ) ->
            ( App Habit context, Cmd.none )

        ( Log, HandleLogMsg subMsg ) ->
            let
                ( nextScreen, nextContext, nextSubMsg ) =
                    updateLogScreen subMsg context
            in
            ( App nextScreen nextContext, Cmd.map HandleLogMsg nextSubMsg )

        ( Log, _ ) ->
            ( App Log context, Cmd.none )

        ( EditHabit, HandleEditHabitsMsg subMsg ) ->
            let
                ( nextScreen, nextContext, nextSubMsg ) =
                    updateEditHabitScreen subMsg context
            in
            ( App nextScreen nextContext, Cmd.map HandleEditHabitsMsg nextSubMsg )

        ( EditHabit, _ ) ->
            ( App EditHabit context, Cmd.none )


updateHabitScreen : HabitMsg -> Context -> ( Screen, Context, Cmd HabitMsg )
updateHabitScreen msg context =
    case msg of
        HabitChangeScreen screen ->
            case screen of
                EditHabit ->
                    ( EditHabit, context, Task.attempt (\_ -> HabitNoOp) (Browser.Dom.focus "edit-habit-input") )

                _ ->
                    ( screen, context, Cmd.none )

        CompleteHabit ->
            ( Habit, context, Task.perform LogHabit Time.now )

        LogHabit now ->
            let
                nextLog =
                    addEntry context.habit now context.habitLog
            in
            ( Habit
            , { context | habitLog = nextLog }
            , saveHabitLog nextLog
            )

        RemoveLastHabitEntry ->
            let
                nextLog =
                    undoLastHabit context.habit context.habitLog
            in
            ( Habit
            , { context | habitLog = nextLog }
            , saveHabitLog nextLog
            )

        HabitNoOp ->
            ( Habit, context, Cmd.none )


updateEditHabitScreen : EditHabitMsg -> Context -> ( Screen, Context, Cmd EditHabitMsg )
updateEditHabitScreen msg context =
    case msg of
        EditHabitChangeScreen screen ->
            ( screen, context, Cmd.none )

        UpdateHabit habit ->
            ( EditHabit
            , { context | habit = habit }
            , saveHabit habit
            )


updateLogScreen : LogMsg -> Context -> ( Screen, Context, Cmd LogMsg )
updateLogScreen msg context =
    case msg of
        LogChangeScreen screen ->
            ( screen, context, Cmd.none )

        LogNoOp ->
            ( Log, context, Cmd.none )



--- PORTS ---


port saveHabitLocally : Encode.Value -> Cmd msg


saveHabit : Habit -> Cmd EditHabitMsg
saveHabit habit =
    saveHabitLocally <| Encode.string habit


port saveHabitLogLocally : Encode.Value -> Cmd msg


saveHabitLog : HabitLog -> Cmd HabitMsg
saveHabitLog habitLog =
    saveHabitLogLocally <| encodeHabitLog habitLog



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model of
        Booting _ ->
            { title = "Building Habits"
            , body = [ text "loading…" ]
            }

        App screen context ->
            { title = "Building Habits"
            , body = [ appContent screen context ]
            }


appContent : Screen -> Context -> Html Msg
appContent screen context =
    div [] [ screenContent screen context ]


screenContent : Screen -> Context -> Html Msg
screenContent screen context =
    case screen of
        Habit ->
            Html.map HandleHabitMsg <| habitScreen context

        EditHabit ->
            Html.map HandleEditHabitsMsg <| editHabitScreen context

        Log ->
            Html.map HandleLogMsg <| logScreen context



---- Habit Screen ----


habitScreen : Context -> Html HabitMsg
habitScreen context =
    div []
        [ habitInfo context
        , progressBar context
        ]


habitInfo : Context -> Html HabitMsg
habitInfo context =
    div [ class "fixed grid h-screen w-screen px-4 py-12 z-10" ]
        [ div [ class "flex flex-col justify-end" ]
            [ habitTextView context
            , habitCountIndicator context
            ]
        , div [ class "flex items-end justify-center" ] [ habitCompleteButton context ]
        , completedTodayText context
        ]


habitCountIndicator : Context -> Html HabitMsg
habitCountIndicator model =
    let
        { habit, habitLog } =
            model

        daysCompletedText =
            String.fromInt
                (timesHabitWasCompleted habit habitLog)
                ++ " / "
                ++ String.fromInt totalDays
    in
    div [ class Typography.body1, onClick <| HabitChangeScreen Log ] [ text daysCompletedText ]


progressBar : Context -> Html HabitMsg
progressBar { habit, habitLog, viewport } =
    let
        height =
            viewport.viewport.height

        completedDays =
            toFloat <| timesHabitWasCompleted habit habitLog

        progressBarHeight =
            (completedDays / toFloat totalDays) * height

        progressBarHeightString =
            (String.fromInt <| round progressBarHeight) ++ "px"
    in
    div
        [ class "fixed bottom-0 left-0 w-full z-0 bg-purple-100"
        , style "height" progressBarHeightString
        ]
        []


habitTextView : Context -> Html HabitMsg
habitTextView { habit, now, habitLog, timeZone } =
    h1
        [ classList
            [ ( Typography.header1 ++ " overflow-y-hidden max-h-64 mb-10 break-anywhere", True )
            , ( "line-through", completedToday timeZone habit now habitLog )
            ]
        , onClick <| HabitChangeScreen EditHabit
        ]
        [ text habit ]


habitCompleteButton : Context -> Html HabitMsg
habitCompleteButton { habit, now, habitLog, timeZone } =
    if completedToday timeZone habit now habitLog then
        div []
            [ button
                [ class <| Buttons.primary ++ " text-gray-800"
                , onClick RemoveLastHabitEntry
                ]
                [ Icons.undo Colors.purple 36 ]
            ]

    else
        button
            [ class <| Buttons.primary ++ " bg-purple-700 hover:bg-purple-800", onClick CompleteHabit ]
            [ Icons.check Colors.white 36 ]


completedTodayText : Context -> Html HabitMsg
completedTodayText { habit, now, habitLog, timeZone } =
    if completedToday timeZone habit now habitLog then
        div
            [ class "absolute w-screen top-0 mt-8 text-gray-600 text-center"
            ]
            [ text "Come back tomorrow!" ]

    else
        text ""



---- Edit Habit Screen ----


editHabitScreen : Context -> Html EditHabitMsg
editHabitScreen model =
    div
        [ classList
            [ ( "absolute z-20 left-0 top-0 bottom-0 right-0 bg-white", True )
            ]
        ]
        [ textarea
            [ value model.habit
            , onInput UpdateHabit
            , classList [ ( Typography.header1, True ), ( "py-8 px-4 resize-none h-screen", True ) ]
            , id "edit-habit-input"
            ]
            []
        , button
            [ class "absolute right-0 bottom-0 mr-8 mb-8 p-4 rounded-full bg-purple-700"
            , onClick <| EditHabitChangeScreen Habit
            ]
            [ div [ class "mx-auto w-min-c" ] [ Icons.save Colors.white 36 ] ]
        ]



---- Log Screen ----


logScreen : Context -> Html LogMsg
logScreen model =
    div [ class "p-4" ]
        [ logScreenHeader
        , habitList model
        ]


logScreenHeader : Html LogMsg
logScreenHeader =
    div [ class "flex flex-row justify-between items-center mb-4" ]
        [ div [ class Typography.header1 ] [ text "Habit Log" ]
        , div [ class Buttons.close, onClick <| LogChangeScreen Habit ] [ text "X" ]
        ]


habitList : Context -> Html msg
habitList model =
    div [] <| List.map (habitListItem model.timeZone) model.habitLog


habitListItem : Time.Zone -> HabitEntry -> Html msg
habitListItem timeZone { date, habit } =
    div [ class "flex flex-row justify-between" ]
        [ div [] [ text habit ]
        , div [] [ text <| formatDate timeZone date ]
        ]


formatDate : Time.Zone -> Posix -> String
formatDate zone date =
    let
        year =
            String.fromInt <| Time.toYear zone date

        month =
            monthToString <| Time.toMonth zone date

        day =
            String.fromInt <| Time.toDay zone date

        hour =
            String.fromInt <| Time.toHour zone date

        minute =
            String.fromInt <| Time.toMinute zone date
    in
    month ++ "/" ++ day ++ "/" ++ year ++ " " ++ hour ++ ":" ++ minute


monthToString : Time.Month -> String
monthToString month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"



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
