module Context exposing (Context)

import Browser.Dom exposing (Viewport)
import Habit exposing (Habit, HabitLog)
import Route exposing (Navigation)
import Time exposing (Posix)


type alias Context =
    { habit : Habit
    , habitLog : HabitLog
    , now : Posix
    , timeZone : Time.Zone
    , viewport : Viewport
    }
