module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src)


---- MODEL ----

type alias Habit =  String

type alias Model =
  { habit: Habit, completedToday: Bool }



init : ( Model, Cmd Msg )
init =
  ( {
    habit = "Log a habit every day"
    , completedToday = False

  }, Cmd.none )



---- UPDATE ----


type Msg
  = CompleteHabit
  | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CompleteHabit ->
      ( { model | completedToday = True } , Cmd.none )
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
    div []
    [
      h1 [] [ text (model.habit) ]
      , text completedText
      , button [ onClick CompleteHabit ] [ text "Done" ]
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
