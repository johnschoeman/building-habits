module Route exposing (Navigation, Route(..), init)


type alias Navigation =
    { currentRoute : Route
    }


type Route
    = Dashboard
    | Analytics
    | AddHabit
    | EditHabit


init : Navigation
init =
    { currentRoute = Dashboard }


navigateTo : Route -> Navigation -> Navigation
navigateTo route oldNav =
    { oldNav | currentRoute = route }
