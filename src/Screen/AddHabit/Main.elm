port module Screen.AddHabit.Main exposing (Model, Msg, init, update, view)

import Context exposing (Context)
import Habit exposing (Habit, HabitLog, encodeHabit)
import Html exposing (Html, button, div, h1, input, label, main_, option, select, span, text)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Encode as Encode
import Route exposing (Navigation, Route(..))
import Screen.ViewHelpers exposing (fixedContent)
import Styles.Colors as Colors
import Styles.Forms as Forms
import Styles.Typography as Typography



---- MODEL ----


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
    , timePreposition = Before
    , timeText = ""
    , placePreposition = In
    , placeText = ""
    }



---- UPDATE ----


type Msg
    = UpdateHabitText String
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
            , nextNav = { nav | currentRoute = Route.Dashboard }
            , nextModel = model
            , nextSubMsg = Cmd.none
            }

        UpdateHabitText text ->
            { nextCtx = ctx
            , nextNav = nav
            , nextModel = { model | habitText = text, errorMsg = "" }
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
                , nextModel = { model | errorMsg = "add an activity" }
                , nextSubMsg = Cmd.none
                }



---- PORTS ----


createHabit : Habit -> Cmd Msg
createHabit habit =
    createHabitLocally <| encodeHabit habit


port createHabitLocally : Encode.Value -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    fixedContent
        [ div [ class "flex flex-col h-full" ]
            [ header model
            , habitForm model
            ]
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


habitForm : Model -> Html Msg
habitForm model =
    div [ class style.habitForm ]
        [ habitInput model
        , timeForm model
        , placeForm model
        ]


habitInput : Model -> Html Msg
habitInput model =
    div [ class "flex flex-row items-baseline" ]
        [ div [ class <| Typography.label ++ " text-center mr-4" ] [ text "I will" ]
        , div
            [ class "flex flex-col flex-grow items-center" ]
            [ div [ class style.inputContainer ]
                [ input
                    [ value model.habitText
                    , class <| Forms.input ++ " w-full"
                    , placeholder "put on running shoes"
                    , attribute "autocapitalize" "none"
                    , onInput UpdateHabitText
                    , id "habit-input"
                    ]
                    []
                ]
            , label [ for "habit-input", class Typography.label ]
                [ text "activity"
                ]
            ]
        ]


timeForm : Model -> Html Msg
timeForm model =
    prepositionInputForm timeRadios timeInput model


timeRadios : Model -> Html Msg
timeRadios model =
    let
        timePrepIsSelected prep =
            model.timePreposition == prep

        updateTimePreposition str =
            UpdateTimePreposition <| stringToTimePreposition str
    in
    radioButtons
        timePrepIsSelected
        showTimePreposition
        updateTimePreposition
        model
        [ Before, TimeAt, After ]


timeInput : Model -> Html Msg
timeInput model =
    madLibInput "time-input" "breakfast" "time of day" model.timeText UpdateTimeText


placeForm : Model -> Html Msg
placeForm model =
    prepositionInputForm placeRadios placeInput model


placeRadios : Model -> Html Msg
placeRadios model =
    let
        placePrepIsSelected prep =
            model.placePreposition == prep

        updatePlacePreposition str =
            UpdatePlacePreposition <| stringToPlacePreposition str
    in
    radioButtons
        placePrepIsSelected
        showPlacePreposition
        updatePlacePreposition
        model
        [ In, PlaceAt, On ]


placeInput : Model -> Html Msg
placeInput model =
    madLibInput "place-input" "my living room" "location" model.placeText UpdatePlaceText


prepositionInputForm : (Model -> Html msg) -> (Model -> Html msg) -> Model -> Html msg
prepositionInputForm radios madlib model =
    div [ class style.prepositionInputForm ]
        [ radios model
        , madlib model
        ]


madLibInput : String -> String -> String -> String -> (String -> msg) -> Html msg
madLibInput i ph subtext v updateInput =
    div
        [ class style.madLibInput ]
        [ div [ class style.inputContainer ]
            [ input
                [ value v
                , class <| Forms.input ++ " w-full"
                , placeholder ph
                , attribute "autocapitalize" "none"
                , onInput updateInput
                , id i
                ]
                []
            ]
        , label [ for i, class Typography.label ]
            [ text subtext
            ]
        ]


radioButtons : (p -> Bool) -> (p -> String) -> (String -> msg) -> Model -> List p -> Html msg
radioButtons isSelected showPreposition updatePrep model prepositions =
    div [ class style.radioButtonsContainer ]
        (List.map (radioButton isSelected showPreposition updatePrep model) prepositions)


radioButton : (p -> Bool) -> (p -> String) -> (String -> msg) -> Model -> p -> Html msg
radioButton isSelected show updatePrep model prep =
    let
        p =
            show prep

        c =
            isSelected prep
    in
    label
        [ classList
            [ ( style.preposition, True )
            , ( style.prepositionSelected, c )
            , ( style.prepositionUnselected, not c )
            ]
        ]
        [ input
            [ type_ "radio"
            , value p
            , name "time"
            , id p
            , onInput updatePrep
            , checked c
            ]
            []
        , span [] [ text p ]
        ]



---- STYLES ----


style =
    { habitForm = "flex flex-col justify-between"
    , prepositionInputForm = "flex flex-col items-baseline w-full"
    , radioButtonsContainer = "self-center pb-2"
    , preposition = "radio-input border border-purple-700 rounded text-sm py-2 px-4 mr-2"
    , prepositionSelected = "bg-purple-700 text-white"
    , prepositionUnselected = "text-gray-900"
    , inputContainer = "border-b border-gray-800 my-4 w-full"
    , madLibInput = "flex flex-col flex-grow items-center w-full"
    }
