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
    , errorMsg : String
    }


init : Context -> Model
init context =
    { habitTitle = context.habit.title, errorMsg = "" }


type Msg
    = UpdateFormInput String
    | SaveHabit



-- ( Route.EditHabit, context, Task.attempt (\_ -> HabitNoOp) (Browser.Dom.focus "edit-habit-input") )


update : Model -> Msg -> Context -> ( Context, Model, Cmd Msg )
update model msg context =
    case msg of
        UpdateFormInput habit ->
            ( context, { model | habitTitle = habit, errorMsg = "" }, Cmd.none )

        SaveHabit ->
            let
                oldNav =
                    context.navigation

                nextNav =
                    { oldNav | currentRoute = Route.Dashboard }

                oldHabit =
                    context.habit

                nextHabit =
                    { oldHabit | title = model.habitTitle }
            in
            if Habit.isValid nextHabit then
                ( { context | habit = nextHabit, navigation = nextNav }
                , model
                , saveHabit nextHabit
                )

            else
                ( context, { model | errorMsg = "Invalid Habit" }, Cmd.none )


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
                    , onClick <| UpdateFormInput ""
                    ]
                    [ div [ class "text-gray-400 font-bold" ] [ text "Clear" ] ]
                , text model.errorMsg
                , button
                    [ class "self-end w-min-c h-min-c py-3 px-4 bg-purple-700 rounded"
                    , onClick <| SaveHabit
                    ]
                    [ div [ class "text-white font-bold" ] [ text "Done" ]
                    ]
                ]
            , textarea
                [ value model.habitTitle
                , onInput UpdateFormInput
                , classList [ ( Typography.header1, True ), ( "resize-none w-full h-full", True ) ]
                , id "edit-habit-input"
                ]
                []
            ]
        ]
