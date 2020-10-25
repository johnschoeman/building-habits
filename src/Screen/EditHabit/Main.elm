port module Screen.EditHabit.Main exposing (Model, Msg, init, update, view)

import Context exposing (Context)
import Habit exposing (Habit, HabitLog, encodeHabit)
import Html exposing (Html, button, div, h1, main_, text, textarea)
import Html.Attributes exposing (class, classList, id, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Encode as Encode
import Route exposing (Navigation, Route(..))
import Screen.ViewHelpers exposing (fixedContent)
import Styles.Buttons as Buttons
import Styles.Colors as Colors
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
    | HandleOnTapCancel


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

        HandleOnTapCancel ->
            { nextCtx = ctx
            , nextNav =
                { nav | currentRoute = Route.Dashboard }
            , nextModel = model
            , nextSubMsg = Cmd.none
            }


updateHabit : Habit -> Cmd Msg
updateHabit habit =
    updateHabitLocally <| encodeHabit habit


port updateHabitLocally : Encode.Value -> Cmd msg


view : Model -> Html Msg
view model =
    fixedContent
        [ div [ class "flex flex-col h-full" ]
            [ header model
            , habitForm model
            ]
        ]


habitForm : Model -> Html Msg
habitForm model =
    div [ class "h-56" ]
        [ textarea
            [ value model.habitTitle
            , onInput UpdateFormInput
            , classList
                [ ( Typography.textInput, True )
                , ( "rounded border p-4 resize-none w-full h-full", True )
                ]
            , id "edit-habit-input"
            ]
            []
        ]


header : Model -> Html Msg
header model =
    div [ class "w-full flex justify-between items-center mb-4 mt-2" ]
        [ button
            [ class "self-start w-min-c h-min-c py-3"
            , onClick HandleOnTapCancel
            ]
            [ div [ class "text-red-400 font-bold" ] [ Icons.x Colors.black 30 ] ]
        , div [ class "h-8" ] [ errorMsg model.errorMsg ]
        , button
            [ class "self-end w-min-c h-min-c py-3"
            , onClick <| SaveHabit
            ]
            [ Icons.check Colors.purple 30
            ]
        ]


errorMsg : String -> Html msg
errorMsg str =
    div [ class <| Typography.error ++ " w-full text-center" ] [ text str ]
