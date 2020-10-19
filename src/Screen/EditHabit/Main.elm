port module Screen.EditHabit.Main exposing (Model, Msg, init, update, view)

import Context exposing (Context)
import Habit exposing (Habit, HabitLog, encodeHabit)
import Html exposing (Html, button, div, h1, main_, text, textarea)
import Html.Attributes exposing (class, classList, id, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Route exposing (Route(..))
import Screen.ViewHelpers exposing (fixedContent)
import Styles.Typography as Typography


type alias Model =
    { habitTitle : String
    }


init : Context -> Model
init context =
    { habitTitle = context.habit.title }


type Msg
    = EditHabitChangeRoute Route
    | UpdateHabitTitle String
    | SubmitSaveHabit


update : Model -> Msg -> Context -> ( Route, Context, Cmd Msg )
update model msg context =
    case msg of
        EditHabitChangeRoute route ->
            ( route, context, Cmd.none )

        UpdateHabitTitle habitTitle ->
            let
                oldHabit =
                    context.habit

                nextHabit =
                    { oldHabit | title = habitTitle }
            in
            ( EditHabit
            , { context | habit = nextHabit }
            , saveHabit nextHabit
            )

        SubmitSaveHabit ->
            ( Dashboard
            , context
            , saveHabit context.habit
            )


port saveHabitLocally : Encode.Value -> Cmd msg


saveHabit : Habit -> Cmd Msg
saveHabit habit =
    saveHabitLocally <| encodeHabit habit


view : Model -> Context -> Html Msg
view model context =
    fixedContent
        [ div [ class "flex flex-col h-full" ]
            [ div [ class "flex justify-between pb-4" ]
                [ button
                    [ class "self-start w-min-c h-min-c py-3 px-4"
                    , onClick <| UpdateHabitTitle ""
                    ]
                    [ div [ class "text-gray-400 font-bold" ] [ text "Clear" ] ]
                , button
                    [ class "self-end w-min-c h-min-c py-3 px-4 bg-purple-700 rounded"
                    , onClick <| SubmitSaveHabit
                    ]
                    [ div [ class "text-white font-bold" ] [ text "Done" ]
                    ]
                ]
            , textarea
                [ value model.habitTitle
                , onInput UpdateHabitTitle
                , classList [ ( Typography.header1, True ), ( "resize-none w-full h-full", True ) ]
                , id "edit-habit-input"
                ]
                []
            ]
        ]
