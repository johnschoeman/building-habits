port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (autofocus, class, classList, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Task
import Time exposing (Month(..), Posix, toDay, toHour, toMinute, toMonth, toYear, utc)
import Url



---- MODEL ----


type alias Habit =
    String


type alias HabitLog =
    { habit : Habit
    , date : Posix
    }


type alias Model =
    { habit : Habit
    , completedToday : Bool
    , habitHistory : List HabitLog
    , editing : Bool
    }


type alias Flags =
    String


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { habit = flags
      , completedToday = False
      , habitHistory = []
      , editing = False
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = CompleteHabit
    | UpdateHabit Habit
    | LogHabit Posix
    | ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompleteHabit ->
            ( { model | completedToday = True }, Task.perform LogHabit Time.now )

        LogHabit now ->
            let
                habitLog =
                    { habit = model.habit, date = now }

                nextHistory =
                    habitLog :: model.habitHistory
            in
            ( { model | completedToday = True, habitHistory = nextHistory }
            , Cmd.none
            )

        UpdateHabit habit ->
            ( { model | habit = habit, completedToday = False }
            , saveHabit habit
            )

        ChangedUrl _ ->
            ( model, Cmd.none )

        ClickedLink _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


port saveUserDataLocally : Encode.Value -> Cmd msg


saveHabit : Habit -> Cmd Msg
saveHabit habit =
    saveUserDataLocally <| Encode.string habit



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Building Habigs"
    , body = [ pageContent model ]
    }


pageContent : Model -> Html Msg
pageContent model =
    let
        completedText =
            if model.completedToday then
                "1 of 21 days"

            else
                "0 of 21 days"
    in
    div [ class "flex flex-col items-center h-screen" ]
        [ div [ class "flex flex-4 flex-col justify-center align-left w-full p-4" ]
            [ habitInput model
            , div [ class secondary ] [ text completedText ]
            , div [] (List.map habitLogToHtml model.habitHistory)
            ]
        , div [ class "flex flex-1 items-center justify-center w-full" ]
            [ button [ class primaryButton, onClick CompleteHabit ] [ text "✓" ]
            ]
        ]


habitInput : Model -> Html Msg
habitInput model =
    input
        [ autofocus True
        , value model.habit
        , onInput UpdateHabit
        , classList [ ( header, True ), ( "py-1", True ) ]
        ]
        []


habitLogToHtml : HabitLog -> Html msg
habitLogToHtml log =
    div [ class secondary ] [ text log.habit, text " - ", text <| toUtcString log.date ]


toUtcString : Posix -> String
toUtcString time =
    String.fromInt (toYear utc time)
        ++ ":"
        ++ toDanishMonth (toMonth utc time)
        ++ ":"
        ++ String.fromInt (toDay utc time)
        ++ ":"
        ++ String.fromInt (toHour utc time)
        ++ ":"
        ++ String.fromInt (toMinute utc time)
        ++ " (UTC)"


toDanishMonth : Month -> String
toDanishMonth month =
    case month of
        Jan ->
            "jan"

        Feb ->
            "feb"

        Mar ->
            "mar"

        Apr ->
            "apr"

        May ->
            "maj"

        Jun ->
            "jun"

        Jul ->
            "jul"

        Aug ->
            "aug"

        Sep ->
            "sep"

        Oct ->
            "okt"

        Nov ->
            "nov"

        Dec ->
            "dec"



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


header : String
header =
    "font-display font-bold leading-tight text-4xl text-gray-900"


secondary : String
secondary =
    "font-body text-gray-700 text-sm"


primaryButton : String
primaryButton =
    "bg-indigo-500 hover:bg-indigo-700 text-white font-bold py-4 px-8 rounded-full"
