port module Screen.EditHabit.Main exposing (Model, Msg, init, update, view)

import Context exposing (Context)
import Habit exposing (Habit, HabitLog, encodeHabit)
import Html exposing (Html, button, div, h1, main_, text, textarea)
import Html.Attributes exposing (class, classList, id, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Route exposing (Navigation, Route(..))
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


type alias NextState =
    { nextCtx : Context
    , nextModel : Model
    , nextNav : Navigation
    , nextSubMsg : Cmd Msg
    }


update : Model -> Msg -> Context -> Navigation -> NextState
update model msg ctx nav =
    case msg of
        UpdateFormInput habit ->
            { nextCtx = ctx
            , nextModel = { model | habitTitle = habit, errorMsg = "" }
            , nextNav = nav
            , nextSubMsg = Cmd.none
            }

        SaveHabit ->
            let
                oldHabit =
                    ctx.habit

                nextHabit =
                    { oldHabit | title = model.habitTitle }
            in
            if Habit.isValid nextHabit then
                { nextCtx = { ctx | habit = nextHabit }
                , nextModel = model
                , nextNav = { nav | currentRoute = Route.Dashboard }
                , nextSubMsg = updateHabit nextHabit
                }

            else
                { nextCtx = ctx
                , nextModel = { model | errorMsg = "Invalid Habit" }
                , nextNav = nav
                , nextSubMsg = Cmd.none
                }


updateHabit : Habit -> Cmd Msg
updateHabit habit =
    updateHabitLocally <| encodeHabit habit


port updateHabitLocally : Encode.Value -> Cmd msg


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
                , div [ class "flex flex-col" ]
                    [ div [ class "text-gray-800" ] [ text "Edit Habit" ]
                    , div [ class "text-red-400" ] [ text model.errorMsg ]
                    ]
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
