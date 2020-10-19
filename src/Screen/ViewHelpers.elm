module Screen.ViewHelpers exposing (fixedContent)

import Html exposing (Html, main_)
import Html.Attributes exposing (class)


fixedContent : List (Html msg) -> Html msg
fixedContent children =
    main_ [ class "h-screen max-w-lg m-auto flex flex-col pt-12 pb-8 px-4" ] children
