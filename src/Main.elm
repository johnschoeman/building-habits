port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (autofocus, class, classList, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
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


type alias HabitHistory =
    List HabitLog


type alias Model =
    { habit : Habit
    , habitHistory : HabitHistory
    , editing : Bool
    , now : Posix
    }


type alias Flags =
    { habit : String
    , habitHistory : Decode.Value
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        habitHistory =
            decodeHabitHistory flags.habitHistory
    in
    ( { habit = flags.habit
      , habitHistory = habitHistory
      , editing = False
      , now = Time.millisToPosix 0
      }
    , Task.perform Now Time.now
    )



---- UPDATE ----


type Msg
    = CompleteHabit
    | UpdateHabit Habit
    | LogHabit Posix
    | ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | Now Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompleteHabit ->
            ( model, Task.perform LogHabit Time.now )

        LogHabit now ->
            let
                habitLog =
                    { habit = model.habit, date = now }

                nextHistory =
                    habitLog :: model.habitHistory
            in
            ( { model | habitHistory = nextHistory }
            , saveHabitHistory nextHistory
            )

        UpdateHabit habit ->
            ( { model | habit = habit }
            , saveHabit habit
            )

        ChangedUrl _ ->
            ( model, Cmd.none )

        ClickedLink _ ->
            ( model, Cmd.none )

        Now now ->
            ( { model | now = now }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


port saveHabitLocally : Encode.Value -> Cmd msg


saveHabit : Habit -> Cmd Msg
saveHabit habit =
    saveHabitLocally <| Encode.string habit


port saveHabitHistoryLocally : Encode.Value -> Cmd msg


saveHabitHistory : HabitHistory -> Cmd Msg
saveHabitHistory habitHistory =
    saveHabitHistoryLocally <| encodeHabitHistory habitHistory


encodeHabitHistory : HabitHistory -> Encode.Value
encodeHabitHistory habitHistory =
    Encode.list encodeHabitLog habitHistory


encodeHabitLog : HabitLog -> Encode.Value
encodeHabitLog log =
    Encode.object
        [ ( "habit", Encode.string log.habit )
        , ( "date", Encode.int (Time.posixToMillis log.date) )
        ]


decodeHabitHistory : Decode.Value -> HabitHistory
decodeHabitHistory value =
    case Decode.decodeValue (Decode.list decodeHabitLog) value of
        Ok history ->
            history

        Err _ ->
            []


decodeHabitLog : Decode.Decoder HabitLog
decodeHabitLog =
    Decode.map2 HabitLog
        (Decode.field "habit" Decode.string)
        (Decode.field "date" Decode.int |> Decode.andThen decodePosix)


decodePosix : Int -> Decode.Decoder Posix
decodePosix v =
    Decode.succeed <| Time.millisToPosix v


completedToday : Model -> Bool
completedToday { habit, habitHistory, now } =
    let
        logCompletedToday log =
            log.habit == habit && isSameDay log.date now
    in
    List.any logCompletedToday habitHistory


isSameDay : Posix -> Posix -> Bool
isSameDay now time =
    (Time.toYear utc now == Time.toYear utc time)
        && (Time.toMonth utc now == Time.toMonth utc time)
        && (Time.toDay utc now == Time.toDay utc time)



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
            if completedToday model then
                "Done"

            else
                " "
    in
    div [ class "flex flex-col items-center h-screen" ]
        [ div [ class "flex flex-4 flex-col justify-center align-left w-full p-4" ]
            [ habitInput model
            , div [ class <| secondary ++ " h-2" ] [ text completedText ]
            , div [ class "pt-4" ] (List.take 10 <| List.map habitLogToHtml model.habitHistory)
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
    div [ class "flex justify-between" ]
        [ div
            [ classList
                [ ( secondary, True )
                , ( "w-full truncate flex flex-2", True )
                ]
            ]
            [ text log.habit ]
        , div [ class <| secondary ++ " flex flex-1" ] [ text <| toUtcString log.date ]
        ]


toUtcString : Posix -> String
toUtcString time =
    toDanishMonth (toMonth utc time)
        ++ "/"
        ++ String.fromInt (toDay utc time)
        ++ "/"
        ++ String.fromInt (toYear utc time)
        ++ " - "
        ++ String.fromInt (toHour utc time)
        ++ ":"
        ++ String.fromInt (toMinute utc time)


toDanishMonth : Month -> String
toDanishMonth month =
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
    "font-body text-gray-700 text-xs"


primaryButton : String
primaryButton =
    "bg-indigo-500 hover:bg-indigo-700 text-white font-bold py-4 px-8 rounded-full"
