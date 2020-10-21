module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Navigation as Nav
import Context exposing (Context)
import Habit exposing (Habit, HabitCompletionEvent, HabitLog, decodeHabit, decodeHabitLog)
import Html exposing (Html, button, div, h1, text, textarea)
import Html.Attributes exposing (class, classList, id, style, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode
import Route
import Screen.Analytics.Main as AnalyticsScreen
import Screen.Dashboard.Main as DashboardScreen
import Screen.EditHabit.Main as EditHabitScreen
import Task
import Time exposing (Posix)
import Url



---- MODEL ----


type alias Flags =
    { habit : Decode.Value
    , habitLog : Decode.Value
    }


type alias BootData =
    { flags : Flags
    }


type alias SystemData =
    { now : Posix
    , zone : Time.Zone
    , viewport : Viewport
    }


type Model
    = Booting BootData
    | App Screen Context


type Screen
    = Dashboard
    | EditHabit EditHabitScreen.Model
    | Analytics


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        getSystemData =
            Task.map3 SystemData
                Time.now
                Time.here
                Browser.Dom.getViewport
    in
    ( Booting { flags = flags }
    , Task.perform GotSystemData getSystemData
    )


buildInitialContext : BootData -> SystemData -> Context
buildInitialContext { flags } { now, zone, viewport } =
    let
        habitLog =
            decodeHabitLog flags.habitLog

        habit =
            decodeHabit flags.habit
    in
    { navigation = { currentRoute = Route.Dashboard }
    , habit = habit
    , habitLog = habitLog
    , now = now
    , timeZone = zone
    , viewport = viewport
    }



---- UPDATE ----


type Msg
    = GotSystemData SystemData
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url
    | ChangedScreen Screen
    | HandleDashboardMsg DashboardScreen.Msg
    | HandleEditHabitsMsg EditHabitScreen.Msg
    | HandleAnalyticsMsg AnalyticsScreen.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Booting bootData, GotSystemData systemData ) ->
            let
                initialContext =
                    buildInitialContext bootData systemData
            in
            ( App Dashboard initialContext, Cmd.none )

        ( Booting bootData, _ ) ->
            ( model, Cmd.none )

        ( App _ context, ChangedUrl _ ) ->
            ( model, Cmd.none )

        ( App _ context, ClickedLink _ ) ->
            ( model, Cmd.none )

        ( App _ context, ChangedScreen screen ) ->
            ( App screen context, Cmd.none )

        ( App screen context, subModelMsg ) ->
            handleScreenMsg screen context subModelMsg


handleScreenMsg : Screen -> Context -> Msg -> ( Model, Cmd Msg )
handleScreenMsg screen context msg =
    case ( screen, msg ) of
        ( Dashboard, HandleDashboardMsg subMsg ) ->
            let
                ( nextContext, nextSubMsg ) =
                    DashboardScreen.update subMsg context
            in
            case nextContext.navigation.currentRoute of
                Route.Dashboard ->
                    ( App Dashboard nextContext
                    , Cmd.map HandleDashboardMsg nextSubMsg
                    )

                nextRoute ->
                    ( App (fromRoute nextRoute nextContext) nextContext
                    , Cmd.map HandleDashboardMsg nextSubMsg
                    )

        ( Analytics, HandleAnalyticsMsg subMsg ) ->
            let
                ( nextContext, nextSubMsg ) =
                    AnalyticsScreen.update subMsg context
            in
            case nextContext.navigation.currentRoute of
                Route.Analytics ->
                    ( App Analytics nextContext
                    , Cmd.map HandleAnalyticsMsg nextSubMsg
                    )

                nextRoute ->
                    ( App (fromRoute nextRoute nextContext) nextContext
                    , Cmd.map HandleAnalyticsMsg nextSubMsg
                    )

        ( EditHabit subModel, HandleEditHabitsMsg subMsg ) ->
            let
                ( nextContext, nextSubModel, nextSubMsg ) =
                    EditHabitScreen.update subModel subMsg context
            in
            case nextContext.navigation.currentRoute of
                Route.EditHabit ->
                    ( App (EditHabit nextSubModel) nextContext
                    , Cmd.map HandleEditHabitsMsg nextSubMsg
                    )

                nextRoute ->
                    ( App (fromRoute nextRoute nextContext) nextContext
                    , Cmd.map HandleEditHabitsMsg nextSubMsg
                    )

        ( Dashboard, _ ) ->
            ( App Dashboard context, Cmd.none )

        ( Analytics, _ ) ->
            ( App Analytics context, Cmd.none )

        ( EditHabit subModel, _ ) ->
            ( App (EditHabit subModel) context, Cmd.none )


fromRoute : Route.Route -> Context -> Screen
fromRoute route context =
    case route of
        Route.Dashboard ->
            Dashboard

        Route.EditHabit ->
            EditHabit <| EditHabitScreen.init context

        Route.Analytics ->
            Analytics



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model of
        Booting _ ->
            { title = "Building Habits"
            , body = [ text "loading…" ]
            }

        App screen context ->
            { title = "Building Habits"
            , body = [ screenContent screen context ]
            }


screenContent : Screen -> Context -> Html Msg
screenContent screen context =
    case screen of
        Dashboard ->
            Html.map HandleDashboardMsg <| DashboardScreen.view context

        EditHabit subModel ->
            Html.map HandleEditHabitsMsg <| EditHabitScreen.view subModel context

        Analytics ->
            Html.map HandleAnalyticsMsg <| AnalyticsScreen.view context



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
