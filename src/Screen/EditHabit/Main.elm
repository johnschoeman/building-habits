port module Screen.EditHabit.Main exposing (Model, Msg, init, update, view)

import Context exposing (Context)
import Habit exposing (Habit, HabitLog, encodeHabit)
import Html exposing (Html, button, div, h1, main_, text, textarea)
import Html.Attributes exposing (class, classList, id, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Route exposing (Navigation, Route(..))
import Screen.ViewHelpers exposing (fixedContent)
import Styles.Buttons as Buttons
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
            [ div [ class "pb-2" ] [ header ]
            , div [ class "h-8" ] [ errorMsg model.errorMsg ]
            , div [ class "h-56" ]
                [ textarea
                    [ value model.habitTitle
                    , onInput UpdateFormInput
                    , classList
                        [ ( Typography.textInput, True )
                        , ( "rounded border p-2 resize-none w-full h-full", True )
                        ]
                    , id "edit-habit-input"
                    ]
                    []
                ]
            ]
        ]


header : Html Msg
header =
    div [ class "w-full flex justify-between items-center" ]
        [ div [ class <| Typography.header1 ++ " w-full text-center" ] [ text "Edit Habit" ]
        , button
            [ class <| "self-end " ++ Buttons.roundedPurple
            , onClick <| SaveHabit
            ]
            [ div [ class "text-white font-bold" ] [ text "Done" ]
            ]
        ]


errorMsg : String -> Html msg
errorMsg str =
    div [ class <| Typography.error ++ " w-full text-center" ] [ text str ]
