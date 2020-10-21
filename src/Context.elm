module Context exposing (Context)

import Browser.Dom exposing (Viewport)
import Habit exposing (Habit, HabitLog)
import Route exposing (Route)
import Time exposing (Posix)


type alias Navigation =
    { currentRoute : Route
    }


type alias Context =
    { navigation : Navigation
    , habit : Habit
    , habitLog : HabitLog
    , now : Posix
    , timeZone : Time.Zone
    , viewport : Viewport
    }
