port module Screen.AddHabit.Main exposing (Model, Msg, init, update, view)

import Context exposing (Context)
import Habit exposing (Habit, HabitLog, encodeHabit)
import Html exposing (Html, button, div, h1, input, label, main_, option, select, text)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Route exposing (Navigation, Route(..))
import Screen.ViewHelpers exposing (fixedContent)
import Styles.Forms as Forms
import Styles.Typography as Typography



---- MODEL ----


type DurationUnit
    = Second
    | Minute
    | Hour


showDurationUnit : DurationUnit -> String
showDurationUnit unit =
    case unit of
        Second ->
            "second(s)"

        Minute ->
            "minute(s)"

        Hour ->
            "hour(s)"


stringToDurationUnit : String -> DurationUnit
stringToDurationUnit str =
    case str of
        "second(s)" ->
            Second

        "minute(s))" ->
            Minute

        "hour(s)" ->
            Hour

        _ ->
            Minute


type TimePreposition
    = Before
    | TimeAt
    | After


showTimePreposition : TimePreposition -> String
showTimePreposition preposition =
    case preposition of
        Before ->
            "before"

        TimeAt ->
            "at"

        After ->
            "after"


stringToTimePreposition : String -> TimePreposition
stringToTimePreposition str =
    case str of
        "before" ->
            Before

        "at" ->
            TimeAt

        "after" ->
            After

        _ ->
            TimeAt


type PlacePreposition
    = In
    | PlaceAt
    | On


showPlacePreposition : PlacePreposition -> String
showPlacePreposition preposition =
    case preposition of
        In ->
            "in"

        PlaceAt ->
            "at"

        On ->
            "on"


stringToPlacePreposition : String -> PlacePreposition
stringToPlacePreposition str =
    case str of
        "in" ->
            In

        "at" ->
            PlaceAt

        "on" ->
            On

        _ ->
            PlaceAt


type alias Model =
    { errorMsg : String
    , habitText : String
    , includeDuration : Bool
    , durationText : String
    , durationUnit : DurationUnit
    , timePreposition : TimePreposition
    , timeText : String
    , placePreposition : PlacePreposition
    , placeText : String
    }


modelToHabit : Model -> Habit
modelToHabit model =
    let
        habitTitle =
            String.join " "
                [ habitPart model
                , durationPart model
                , timePart model
                , placePart model
                ]
                |> String.trim
    in
    { id = slugify habitTitle, title = habitTitle }


habitPart : Model -> String
habitPart model =
    String.join " "
        [ "I will"
        , model.habitText
        ]


durationPart : Model -> String
durationPart model =
    if model.includeDuration && String.length model.durationText > 0 then
        String.join " "
            [ "for"
            , model.durationText
            , showDurationUnit model.durationUnit
            ]

    else
        ""


timePart : Model -> String
timePart model =
    if String.length model.timeText > 0 then
        String.join " "
            [ showTimePreposition model.timePreposition
            , model.timeText
            ]

    else
        ""


placePart : Model -> String
placePart model =
    if String.length model.placeText > 0 then
        String.join " "
            [ showPlacePreposition model.placePreposition
            , model.placeText
            ]

    else
        ""


slugify : String -> String
slugify string =
    String.replace " " "-" string
        |> String.toLower


init : Context -> Model
init context =
    { errorMsg = ""
    , habitText = ""
    , includeDuration = True
    , durationText = ""
    , durationUnit = Second
    , timePreposition = Before
    , timeText = ""
    , placePreposition = In
    , placeText = ""
    }



---- UPDATE ----


type Msg
    = UpdateHabitText String
    | ToggleShowDuration
    | UpdateDurationText String
    | UpdateDurationUnit DurationUnit
    | UpdateTimePreposition TimePreposition
    | UpdateTimeText String
    | UpdatePlacePreposition PlacePreposition
    | UpdatePlaceText String
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

        UpdateHabitText text ->
            { nextCtx = ctx
            , nextNav = nav
            , nextModel = { model | habitText = text, errorMsg = "" }
            , nextSubMsg = Cmd.none
            }

        ToggleShowDuration ->
            { nextCtx = ctx
            , nextNav = nav
            , nextModel = { model | includeDuration = not model.includeDuration }
            , nextSubMsg = Cmd.none
            }

        UpdateDurationText text ->
            { nextCtx = ctx
            , nextNav = nav
            , nextModel = { model | durationText = text }
            , nextSubMsg = Cmd.none
            }

        UpdateDurationUnit unit ->
            { nextCtx = ctx
            , nextNav = nav
            , nextModel = { model | durationUnit = unit }
            , nextSubMsg = Cmd.none
            }

        UpdateTimePreposition preposition ->
            { nextCtx = ctx
            , nextNav = nav
            , nextModel = { model | timePreposition = preposition }
            , nextSubMsg = Cmd.none
            }

        UpdateTimeText text ->
            { nextCtx = ctx
            , nextNav = nav
            , nextModel = { model | timeText = text }
            , nextSubMsg = Cmd.none
            }

        UpdatePlacePreposition preposition ->
            { nextCtx = ctx
            , nextNav = nav
            , nextModel = { model | placePreposition = preposition }
            , nextSubMsg = Cmd.none
            }

        UpdatePlaceText text ->
            { nextCtx = ctx
            , nextNav = nav
            , nextModel = { model | placeText = text }
            , nextSubMsg = Cmd.none
            }

        SaveHabit ->
            let
                nextHabit =
                    modelToHabit model
            in
            if String.length model.habitText > 0 then
                { nextCtx = { ctx | habit = nextHabit }
                , nextNav = { nav | currentRoute = Route.Dashboard }
                , nextModel = model
                , nextSubMsg = createHabit nextHabit
                }

            else
                { nextCtx = ctx
                , nextNav = nav
                , nextModel = { model | errorMsg = "habit required" }
                , nextSubMsg = Cmd.none
                }



---- PORTS ----


createHabit : Habit -> Cmd Msg
createHabit habit =
    createHabitLocally <| encodeHabit habit


port createHabitLocally : Encode.Value -> Cmd msg



---- VIEW ----


view : Model -> Context -> Html Msg
view model context =
    fixedContent
        [ div [ class "flex flex-col h-full" ]
            [ div [ class "pb-2" ] [ header ]
            , div [ class "h-8" ] [ errorMsg model.errorMsg ]
            , habitForm model
            ]
        ]


header : Html Msg
header =
    div [ class "w-full flex justify-between items-center" ]
        [ button
            [ class "self-start w-min-c h-min-c py-3 px-4"
            , onClick HandleOnTapCancel
            ]
            [ div [ class "text-red-400 font-bold" ] [ text "Cancel" ] ]
        , div [ class Typography.header2 ] [ text "Build Habit" ]
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


habitForm : Model -> Html Msg
habitForm model =
    div [ class "flex flex-col justify-between h-56" ]
        [ habitInput model
        , durationForm model
        , timeForm model
        , placeForm model
        ]


habitInput : Model -> Html Msg
habitInput model =
    div [ class "flex flex-row" ]
        [ div [ class <| Typography.body1 ++ " mr-4" ] [ text "I will" ]
        , input
            [ value model.habitText
            , class <| Forms.input ++ " flex-grow"
            , placeholder "habit"
            , attribute "autocapitalize" "none"
            , onInput UpdateHabitText
            ]
            []
        ]


durationForm : Model -> Html Msg
durationForm model =
    div
        [ classList
            [ ( "flex flex-row items-baseline", True )
            , ( "text-gray-600 line-through", not model.includeDuration )
            ]
        ]
        [ div [ class <| Typography.body1 ++ " mr-4" ] [ text "for" ]
        , div [ class "mr-4" ] [ durationInput model ]
        , div [] [ durationUnitSelect model ]
        , div [ class "w-full h-full flex flex-row items-center justify-end" ] [ durationCheckbox model ]
        ]


durationInput : Model -> Html Msg
durationInput model =
    input
        [ value model.durationText
        , placeholder "duration"
        , class <| Forms.input ++ " w-32"
        , type_ "number"
        , onInput UpdateDurationText
        , attribute "autocapitalize" "none"
        , disabled <| not model.includeDuration
        ]
        []


durationUnitSelect : Model -> Html Msg
durationUnitSelect model =
    select
        [ class Forms.select
        , value <| showDurationUnit model.durationUnit
        , onInput (\str -> UpdateDurationUnit <| stringToDurationUnit str)
        , disabled <| not model.includeDuration
        ]
        [ durationUnitOption Second
        , durationUnitOption Minute
        , durationUnitOption Hour
        ]


durationUnitOption : DurationUnit -> Html Msg
durationUnitOption duration =
    option [ value <| showDurationUnit duration ] [ text <| showDurationUnit duration ]


durationCheckbox : Model -> Html Msg
durationCheckbox model =
    let
        checkboxIdentifier =
            "include-duration"
    in
    input
        [ type_ "checkbox"
        , class "border h-6 w-6 text-gray-800"
        , checked model.includeDuration
        , onClick ToggleShowDuration
        ]
        []


timeForm : Model -> Html Msg
timeForm model =
    div [ class "flex flex-row items-baseline" ]
        [ timeSelect model
        , timeInput model
        ]


timeSelect : Model -> Html Msg
timeSelect model =
    select
        [ class <| Forms.select ++ " w-24 mr-4"
        , value <| showTimePreposition model.timePreposition
        , onInput (\str -> UpdateTimePreposition <| stringToTimePreposition str)
        ]
        [ timeOption Before
        , timeOption TimeAt
        , timeOption After
        ]


timeOption : TimePreposition -> Html Msg
timeOption preposition =
    option [ value <| showTimePreposition preposition ] [ text <| showTimePreposition preposition ]


timeInput : Model -> Html Msg
timeInput model =
    input
        [ value model.timeText
        , class <| Forms.input
        , placeholder "time of day"
        , attribute "autocapitalize" "none"
        , onInput UpdateTimeText
        ]
        []


placeForm : Model -> Html Msg
placeForm model =
    div [ class "flex flex-row items-baseline" ]
        [ placeSelect model
        , placeInput model
        ]


placeSelect : Model -> Html Msg
placeSelect model =
    select
        [ class <| Forms.select ++ " w-24 mr-4"
        , value <| showPlacePreposition model.placePreposition
        , onInput (\str -> UpdatePlacePreposition <| stringToPlacePreposition str)
        ]
        [ placeOption In
        , placeOption PlaceAt
        , placeOption On
        ]


placeInput : Model -> Html Msg
placeInput model =
    input
        [ value model.placeText
        , class Forms.input
        , placeholder "location"
        , attribute "autocapitalize" "none"
        , onInput UpdatePlaceText
        ]
        []


placeOption : PlacePreposition -> Html Msg
placeOption preposition =
    option [ value <| showPlacePreposition preposition ] [ text <| showPlacePreposition preposition ]
