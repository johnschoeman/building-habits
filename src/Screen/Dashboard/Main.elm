port module Screen.Dashboard.Main exposing (Msg, update, view)

import Browser.Dom exposing (Viewport)
import Context exposing (Context)
import Habit
    exposing
        ( HabitLog
        , addEntry
        , completedToday
        , encodeHabitLog
        , timesHabitWasCompleted
        , undoLastHabit
        )
import Html exposing (Html, button, div, h1, text, textarea)
import Html.Attributes exposing (class, classList, id, style, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Encode as Encode
import Route exposing (Route)
import Screen.ViewHelpers exposing (fixedContent)
import Styles.Buttons as Buttons
import Styles.Colors as Colors
import Styles.Typography as Typography
import Task
import Time exposing (Posix)


type Msg
    = DashboardChangeRoute Route
    | CompleteHabit
    | LogHabit Posix
    | RemoveLastHabitEntry
    | HabitNoOp


totalDays =
    21


update : Msg -> Context -> ( Route.Route, Context, Cmd Msg )
update msg context =
    case msg of
        DashboardChangeRoute route ->
            case route of
                Route.EditHabit ->
                    ( Route.EditHabit, context, Task.attempt (\_ -> HabitNoOp) (Browser.Dom.focus "edit-habit-input") )

                _ ->
                    ( route, context, Cmd.none )

        CompleteHabit ->
            ( Route.Dashboard, context, Task.perform LogHabit Time.now )

        LogHabit now ->
            let
                nextLog =
                    addEntry context.habit now context.habitLog
            in
            ( Route.Dashboard
            , { context | habitLog = nextLog }
            , saveHabitLog nextLog
            )

        RemoveLastHabitEntry ->
            let
                nextLog =
                    undoLastHabit context.habit context.habitLog
            in
            ( Route.Dashboard
            , { context | habitLog = nextLog }
            , saveHabitLog nextLog
            )

        HabitNoOp ->
            ( Route.Dashboard, context, Cmd.none )


port saveHabitLogLocally : Encode.Value -> Cmd msg


saveHabitLog : HabitLog -> Cmd Msg
saveHabitLog habitLog =
    saveHabitLogLocally <| encodeHabitLog habitLog


view : Context -> Html Msg
view context =
    if Habit.valid context.habit then
        fixedContent
            [ habitInfo context
            , progressBar context
            ]

    else
        fixedContent [ createHabitButton ]


createHabitButton : Html Msg
createHabitButton =
    div [ class "flex justify-center items-center h-full" ]
        [ button
            [ class <| "text-white font-bold px-6 py-4 rounded bg-purple-700 hover:bg-purple-800"
            , onClick <| DashboardChangeRoute Route.EditHabit
            ]
            [ text "Add a habit" ]
        ]


habitInfo : Context -> Html Msg
habitInfo context =
    div [ class "grid grid-rows-5 max-h-full flex-grow z-10" ]
        [ div [ class "row-start-1 row-end-2 flex justify-center" ]
            [ habitHeader context ]
        , div [ class "row-start-2 row-end-5 flex flex-col justify-center max-h-full" ]
            [ habitTextView context
            , habitCountIndicator context
            ]
        , div [ class "row-start-5 row-end-6 flex flex-col items-center justify-center" ]
            [ habitCompleteButton context ]
        ]


progressBar : Context -> Html Msg
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
        [ class "fixed bottom-0 left-0 w-full z-0 bg-purple-100 transition-height duration-1000 ease-in-out"
        , style "height" progressBarHeightString
        ]
        []


habitTextView : Context -> Html Msg
habitTextView { habit, now, habitLog, timeZone } =
    div [ class "flex flex-col justify-end mb-4 max-h-full" ]
        [ h1
            [ classList
                [ ( Typography.header1 ++ " break-anywhere overflow-y-scroll", True )
                , ( "line-through", completedToday timeZone habit now habitLog )
                ]
            , onClick <| DashboardChangeRoute Route.EditHabit
            ]
            [ text habit.title ]
        ]


habitCountIndicator : Context -> Html Msg
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
    div [ class "" ]
        [ div [ class Typography.body1 ] [ text daysCompletedText ]
        ]


habitCompleteButton : Context -> Html Msg
habitCompleteButton { habit, now, habitLog, timeZone } =
    if completedToday timeZone habit now habitLog then
        button [ class <| Buttons.primary ++ " text-gray-800", onClick RemoveLastHabitEntry ]
            [ Icons.undo Colors.purple 36 ]

    else
        button
            [ class <| Buttons.primary ++ " bg-purple-700 hover:bg-purple-800", onClick CompleteHabit ]
            [ Icons.check Colors.white 36 ]


habitHeader : Context -> Html Msg
habitHeader context =
    div [ class "h-4" ]
        [ completedTodayText context
        ]


completedTodayText : Context -> Html Msg
completedTodayText { habit, now, habitLog, timeZone } =
    if completedToday timeZone habit now habitLog then
        text "Come back tomorrow!"

    else
        text " "
