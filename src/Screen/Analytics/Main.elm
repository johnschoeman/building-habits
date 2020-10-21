module Screen.Analytics.Main exposing (Msg, update, view)

import Context exposing (Context)
import Habit exposing (Habit, HabitCompletionEvent, HabitLog, decodeHabit, decodeHabitLog)
import Html exposing (Html, button, div, h1, text, textarea)
import Html.Attributes exposing (class, classList, id, style, value)
import Html.Events exposing (onClick, onInput)
import Route exposing (Route(..))
import Screen.ViewHelpers exposing (fixedContent)
import Styles.Buttons as Buttons
import Styles.Colors as Colors
import Styles.Typography as Typography
import Time exposing (Month(..), Posix)


type Msg
    = LogChangeScreen Route
    | NoOp


update : Msg -> Context -> ( Context, Cmd Msg )
update msg context =
    case msg of
        LogChangeScreen route ->
            let
                oldNav =
                    context.navigation

                nextNav =
                    { oldNav | currentRoute = route }
            in
            ( { context | navigation = nextNav }, Cmd.none )

        NoOp ->
            ( context, Cmd.none )


view : Context -> Html Msg
view model =
    fixedContent
        [ logScreenHeader
        , habitList model
        ]


logScreenHeader : Html Msg
logScreenHeader =
    div [ class "flex flex-row justify-between items-center mb-4" ]
        [ div [ class Typography.header1 ] [ text "Habit Log" ]
        , div [ class Buttons.close, onClick <| LogChangeScreen Dashboard ] [ text "X" ]
        ]


habitList : Context -> Html msg
habitList model =
    div [] <| List.map (habitListItem model.timeZone) model.habitLog


habitListItem : Time.Zone -> HabitCompletionEvent -> Html msg
habitListItem timeZone { date, habit } =
    div [ class "flex flex-row justify-between" ]
        [ div [] [ text habit.title ]
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
