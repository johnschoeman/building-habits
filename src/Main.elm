port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (autofocus, class, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Url



---- MODEL ----


type alias Habit =
    String


type alias Model =
    { habit : Habit
    , completedToday : Bool
    , editing : Bool
    }


type alias Flags =
    String


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { habit = flags
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
    | ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompleteHabit ->
            ( { model | completedToday = True }, Cmd.none )

        EditHabit ->
            ( { model | editing = True }, Cmd.none )

        UpdateHabit habit ->
            ( { model | habit = habit }
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
