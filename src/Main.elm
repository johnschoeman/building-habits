module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (autofocus, class, placeholder, src, value)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Habit =
    String


type alias Model =
    { habit : Habit
    , completedToday : Bool
    , editing : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { habit = "Log a habit every day"
      , completedToday = False
      , editing = False
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = CompleteHabit
    | EditHabit
    | UpdateHabit Habit
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompleteHabit ->
            ( { model | completedToday = True }, Cmd.none )

        EditHabit ->
            ( { model | editing = True }, Cmd.none )

        UpdateHabit habit ->
            ( { model | habit = habit }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        completedText =
            if model.completedToday then
                "Done"

            else
                "Not Done"
    in
    div [ class "flex flex-col items-center" ]
        [ input
            [ placeholder model.habit
            , autofocus True
            , value model.habit
            , onInput UpdateHabit
            ]
            []
        , div [] [ text completedText ]
        , button [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded", onClick CompleteHabit ] [ text "Done" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
