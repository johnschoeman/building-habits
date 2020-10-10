port module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Habit
    exposing
        ( Habit
        , HabitEntry
        , HabitLog
        , completedToday
        , decodeHabitLog
        , encodeHabitLog
        , timesHabitWasCompleted
        )
import Html exposing (Html, button, div, h1, text, textarea)
import Html.Attributes exposing (class, classList, id, style, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Time exposing (Month(..), Posix)
import Url



---- MODEL ----


type alias ViewportSize =
    { height : Float
    }


type alias Model =
    { habit : Habit
    , habitLog : HabitLog
    , editing : Bool
    , showLog : Bool
    , now : Posix
    , timeZone : Time.Zone
    , viewportSize : ViewportSize
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
      , showLog = True
      , now = Time.millisToPosix 0
      , timeZone = Time.utc
      , viewportSize = ViewportSize 0
      }
    , Cmd.batch
        [ Task.perform Now Time.now
        , Task.perform Zone Time.here
        , Task.perform GetViewport Browser.Dom.getViewport
        ]
    )


totalDays =
    21



---- UPDATE ----


type Msg
    = CompleteHabit
    | RemoveLastHabitEntry
    | UpdateHabit Habit
    | StartEditHabit
    | EndEditHabit
    | ToggleShowLog
    | LogHabit Posix
    | ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | Now Posix
    | Zone Time.Zone
    | GetViewport Browser.Dom.Viewport
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

        StartEditHabit ->
            let
                focus =
                    Task.attempt (\_ -> NoOp) (Browser.Dom.focus "edit-habit-input")
            in
            ( { model | editing = True }, focus )

        EndEditHabit ->
            ( { model | editing = False }, Cmd.none )

        ToggleShowLog ->
            ( { model | showLog = not model.showLog }, Cmd.none )

        ChangedUrl _ ->
            ( model, Cmd.none )

        ClickedLink _ ->
            ( model, Cmd.none )

        Now now ->
            ( { model | now = now }, Cmd.none )

        Zone zone ->
            ( { model | timeZone = zone }, Cmd.none )

        GetViewport viewport ->
            let
                viewportSize =
                    model.viewportSize

                nextViewportSize =
                    { viewportSize | height = viewport.viewport.height }
            in
            ( { model | viewportSize = nextViewportSize }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



--- Ports ---


port saveHabitLocally : Encode.Value -> Cmd msg


saveHabit : Habit -> Cmd Msg
saveHabit habit =
    saveHabitLocally <| Encode.string habit


port saveHabitLogLocally : Encode.Value -> Cmd msg


saveHabitLog : HabitLog -> Cmd Msg
saveHabitLog habitLog =
    saveHabitLogLocally <| encodeHabitLog habitLog



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Building Habits"
    , body = [ pageContent model ]
    }


pageContent : Model -> Html Msg
pageContent model =
    div []
        [ mainScreen model
        , progressBar model
        , editHabitModal model
        ]


mainScreen : Model -> Html Msg
mainScreen model =
    if model.showLog then
        logScreen model

    else
        habitScreen model



---- Habit Screen ----


habitScreen : Model -> Html Msg
habitScreen model =
    div [ class "fixed grid h-screen w-screen px-4 py-12 z-10" ]
        [ div [ class "flex flex-col justify-end" ]
            [ habitTextView model
            , habitCountIndicator model
            ]
        , div [ class "flex items-end justify-center" ] [ habitCompleteButton model ]
        , completedTodayText model
        ]


habitCountIndicator : Model -> Html Msg
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
    div [ class daysText, onClick ToggleShowLog ] [ text daysCompletedText ]


progressBar : Model -> Html Msg
progressBar { habit, habitLog, viewportSize } =
    let
        completedDays =
            toFloat <| timesHabitWasCompleted habit habitLog

        progressBarHeight =
            (completedDays / toFloat totalDays) * viewportSize.height

        progressBarHeightString =
            (String.fromInt <| round progressBarHeight) ++ "px"
    in
    div
        [ class "fixed bottom-0 left-0 w-full z-0 bg-purple-100"
        , style "height" progressBarHeightString
        ]
        []


habitTextView : Model -> Html Msg
habitTextView { habit, now, habitLog, timeZone } =
    h1
        [ classList
            [ ( header ++ " overflow-y-hidden max-h-64 mb-10 break-anywhere", True )
            , ( "line-through", completedToday timeZone habit now habitLog )
            ]
        , onClick StartEditHabit
        ]
        [ text habit ]


habitCompleteButton : Model -> Html Msg
habitCompleteButton { habit, now, habitLog, timeZone } =
    if completedToday timeZone habit now habitLog then
        div []
            [ button
                [ class <| primaryButton ++ " text-gray-800"
                , onClick RemoveLastHabitEntry
                ]
                [ Icons.undo purple 36 ]
            ]

    else
        button
            [ class <| primaryButton ++ " bg-purple-700 hover:bg-purple-800", onClick CompleteHabit ]
            [ Icons.check white 36 ]


completedTodayText : Model -> Html Msg
completedTodayText { habit, now, habitLog, timeZone } =
    if completedToday timeZone habit now habitLog then
        div
            [ class "absolute w-screen top-0 mt-8 text-gray-600 text-center"
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
            , id "edit-habit-input"
            ]
            []
        , button
            [ class "absolute right-0 bottom-0 mr-8 mb-8 p-4 rounded-full bg-purple-700"
            , onClick EndEditHabit
            ]
            [ div [ class "mx-auto w-min-c" ] [ Icons.save white 36 ] ]
        ]



---- Log Screen ----


logScreen : Model -> Html Msg
logScreen model =
    div [ class "p-4" ]
        [ logScreenHeader
        , habitList model
        ]


logScreenHeader : Html Msg
logScreenHeader =
    div [ class "flex flex-row justify-between items-center mb-4" ]
        [ div [ class header ] [ text "Habit Log" ]
        , div [ class closeButton, onClick ToggleShowLog ] [ text "X" ]
        ]


habitList : Model -> Html msg
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


closeButton : String
closeButton =
    "font-bold p-4 rounded-full"


primaryButton : String
primaryButton =
    "text-white font-bold p-4 rounded-full"
