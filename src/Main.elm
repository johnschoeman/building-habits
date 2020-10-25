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
import Route exposing (Navigation)
import Screen.AddHabit.Main as AddHabitScreen
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
    | App Screen Context Navigation


type Screen
    = Dashboard
    | Analytics
    | AddHabit AddHabitScreen.Model
    | EditHabit EditHabitScreen.Model


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
    { habit = habit
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
    | HandleDashboardMsg DashboardScreen.Msg
    | HandleEditHabitsMsg EditHabitScreen.Msg
    | HandleAddHabitMsg AddHabitScreen.Msg
    | HandleAnalyticsMsg AnalyticsScreen.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Booting bootData, GotSystemData systemData ) ->
            let
                initialContext =
                    buildInitialContext bootData systemData
            in
            ( App Dashboard initialContext Route.init, Cmd.none )

        ( Booting bootData, _ ) ->
            ( model, Cmd.none )

        ( App _ _ _, ChangedUrl _ ) ->
            ( model, Cmd.none )

        ( App _ _ _, ClickedLink _ ) ->
            ( model, Cmd.none )

        ( App screen context navigation, subModelMsg ) ->
            handleScreenMsg screen context navigation subModelMsg


handleScreenMsg : Screen -> Context -> Navigation -> Msg -> ( Model, Cmd Msg )
handleScreenMsg screen ctx nav msg =
    case ( screen, msg ) of
        ( Dashboard, HandleDashboardMsg subMsg ) ->
            let
                { nextCtx, nextNav, nextSubMsg } =
                    DashboardScreen.update subMsg ctx nav
            in
            handleRouteMsg Route.Dashboard
                nextNav
                nextCtx
                Dashboard
                (Cmd.map HandleDashboardMsg nextSubMsg)

        ( Analytics, HandleAnalyticsMsg subMsg ) ->
            let
                { nextCtx, nextNav, nextSubMsg } =
                    AnalyticsScreen.update subMsg ctx nav
            in
            handleRouteMsg Route.Analytics
                nextNav
                nextCtx
                Analytics
                (Cmd.map HandleAnalyticsMsg nextSubMsg)

        ( AddHabit subModel, HandleAddHabitMsg subMsg ) ->
            let
                { nextCtx, nextNav, nextModel, nextSubMsg } =
                    AddHabitScreen.update subModel subMsg ctx nav
            in
            handleRouteMsg Route.AddHabit
                nextNav
                nextCtx
                (AddHabit nextModel)
                (Cmd.map HandleAddHabitMsg nextSubMsg)

        ( EditHabit subModel, HandleEditHabitsMsg subMsg ) ->
            let
                { nextCtx, nextNav, nextModel, nextSubMsg } =
                    EditHabitScreen.update subModel subMsg ctx nav
            in
            handleRouteMsg Route.EditHabit
                nextNav
                nextCtx
                (EditHabit nextModel)
                (Cmd.map HandleEditHabitsMsg nextSubMsg)

        ( Dashboard, _ ) ->
            ( App Dashboard ctx nav, Cmd.none )

        ( Analytics, _ ) ->
            ( App Analytics ctx nav, Cmd.none )

        ( EditHabit subModel, _ ) ->
            ( App (EditHabit subModel) ctx nav, Cmd.none )

        ( AddHabit subModel, _ ) ->
            ( App (AddHabit subModel) ctx nav, Cmd.none )


handleRouteMsg : Route.Route -> Navigation -> Context -> Screen -> Cmd Msg -> ( Model, Cmd Msg )
handleRouteMsg currentRoute nextNav nextCtx nextScreen nextMsg =
    if currentRoute == nextNav.currentRoute then
        ( App nextScreen nextCtx nextNav, nextMsg )

    else
        ( App (fromRoute nextNav.currentRoute nextCtx) nextCtx nextNav, nextMsg )


fromRoute : Route.Route -> Context -> Screen
fromRoute route context =
    case route of
        Route.Dashboard ->
            Dashboard

        Route.Analytics ->
            Analytics

        Route.EditHabit ->
            EditHabit <| EditHabitScreen.init context

        Route.AddHabit ->
            AddHabit <| AddHabitScreen.init context



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model of
        Booting _ ->
            { title = "Building Habits"
            , body = [ text "loading…" ]
            }

        App screen context navigation ->
            { title = "Building Habits"
            , body = [ screenContent screen context ]
            }


screenContent : Screen -> Context -> Html Msg
screenContent screen context =
    case screen of
        Dashboard ->
            Html.map HandleDashboardMsg <| DashboardScreen.view context

        Analytics ->
            Html.map HandleAnalyticsMsg <| AnalyticsScreen.view context

        AddHabit subModel ->
            Html.map HandleAddHabitMsg <| AddHabitScreen.view subModel

        EditHabit subModel ->
            Html.map HandleEditHabitsMsg <| EditHabitScreen.view subModel



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
