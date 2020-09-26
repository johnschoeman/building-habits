port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (autofocus, class, classList, placeholder, src, value)
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
    | UpdateHabit Habit
    | ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompleteHabit ->
            ( { model | completedToday = True }, Cmd.none )

        UpdateHabit habit ->
            ( { model | habit = habit, completedToday = False }
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
                "1 of 21 days"

            else
                "0 of 21 days"
    in
    div [ class "flex flex-col items-center h-screen" ]
        [ div [ class "flex flex-4 flex-col justify-center align-left w-full p-4" ]
            [ habitInput model
            , div [ class secondary ] [ text completedText ]
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
    "font-body text-gray-700 text-sm"


primaryButton : String
primaryButton =
    "bg-indigo-500 hover:bg-indigo-700 text-white font-bold py-4 px-8 rounded-full"
