port module Screen.AddHabit.Main exposing (Model, Msg, init, update, view)

import Context exposing (Context)
import Habit exposing (Habit, HabitLog, encodeHabit)
import Html exposing (Html, button, div, h1, main_, text, textarea)
import Html.Attributes exposing (class, classList, id, placeholder, value)
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
    { habitTitle = "", errorMsg = "" }


type Msg
    = UpdateFormInput String
    | SaveHabit
    | HandleOnTapCancel


type alias NextState =
    { nextCtx : Context
    , nextNav : Navigation
    , nextModel : Model
    , nextSubMsg : Cmd Msg
    }


update : Model -> Msg -> Context -> Navigation -> NextState
update model msg ctx nav =
    case msg of
        HandleOnTapCancel ->
            { nextCtx = ctx
            , nextNav =
                { nav | currentRoute = Route.Dashboard }
            , nextModel = model
            , nextSubMsg = Cmd.none
            }

        UpdateFormInput habit ->
            { nextCtx = ctx
            , nextNav = nav
            , nextModel = { model | habitTitle = habit, errorMsg = "" }
            , nextSubMsg = Cmd.none
            }

        SaveHabit ->
            let
                nextHabit =
                    { id = slugify model.habitTitle, title = model.habitTitle }
            in
            if Habit.isValid nextHabit then
                { nextCtx = { ctx | habit = nextHabit }
                , nextNav = { nav | currentRoute = Route.Dashboard }
                , nextModel = model
                , nextSubMsg = createHabit nextHabit
                }

            else
                { nextCtx = ctx
                , nextNav = nav
                , nextModel = { model | errorMsg = "Invalid Habit" }
                , nextSubMsg = Cmd.none
                }


slugify : String -> String
slugify string =
    String.replace " " "" string
        |> String.toLower


createHabit : Habit -> Cmd Msg
createHabit habit =
    createHabitLocally <| encodeHabit habit


port createHabitLocally : Encode.Value -> Cmd msg


view : Model -> Context -> Html Msg
view model context =
    fixedContent
        [ div [ class "flex flex-col h-full" ]
            [ div [ class "pb-2" ] [ header ]
            , div [ class "h-8" ] [ errorMsg model.errorMsg ]
            , div [ class "h-56" ]
                [ textarea
                    [ value model.habitTitle
                    , placeholder placeholderText
                    , onInput UpdateFormInput
                    , classList
                        [ ( Typography.textInput, True )
                        , ( "rounded border border-gray-400 p-2 resize-none w-full max-h-full min-h-full", True )
                        ]
                    , id "edit-habit-input"
                    ]
                    []
                ]
            ]
        ]


placeholderText : String
placeholderText =
    "I will meditate for 10 seconds before bed in my living room..."


header : Html Msg
header =
    div [ class "w-full flex justify-between items-center" ]
        [ button
            [ class "self-start w-min-c h-min-c py-3 px-4"
            , onClick HandleOnTapCancel
            ]
            [ div [ class "text-red-400 font-bold" ] [ text "Cancel" ] ]
        , div [ class Typography.header1 ] [ text "Build Habit" ]
        , button
            [ class "self-end w-min-c h-min-c py-3 px-4 bg-purple-700 rounded"
            , onClick <| SaveHabit
            ]
            [ div [ class "text-white font-bold" ] [ text "Done" ]
            ]
        ]


errorMsg : String -> Html msg
errorMsg str =
    div [ class <| Typography.error ++ " w-full text-center" ] [ text str ]
