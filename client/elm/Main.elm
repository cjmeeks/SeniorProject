module Main exposing (..)

import AddWorkout.View as Add
import Bootstrap.Navbar as Nav
import Dashboard.View as Dash
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Init exposing (initUser, initialModel)
import Nav.View as NavView
import Navigation exposing (Location)
import Ports.Ports exposing (DatePickerMsg, buildDatePicker, handleDateChange)
import Random
import Rest exposing (getUser)
import Routing exposing (parseLocation)
import Stats.Update
import Stats.View as Stats
import Types exposing (Model, Msg(..), Page(..))
import User.View as User
import Workout.View as Workout


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavMsg state ->
            ( { model | navbar = state }, Cmd.none )

        HandleUser us ->
            ( { model | user = us }, Cmd.none )

        HandleError err ->
            let
                temp =
                    Debug.log "Error: " err
            in
            ( model, Cmd.none )

        HandleDateChange date ->
            ( { model | queryDate = date }, Cmd.none )

        LocationChange loc ->
            ( { model | currentPage = parseLocation loc }, Cmd.none )

        StatsUpdate msg ->
            let
                ( updatedModel, cmd ) =
                    Stats.Update.update msg model
            in
            ( updatedModel, Cmd.map StatsUpdate cmd )
        
        AddExercise ->
            (model, Cmd.none)

        AddRun ->
            (model, Cmd.none)


init : Location -> ( Model, Cmd Msg )
init loc =
    let
        initModel =
            initialModel loc
    in
    ( initModel, Cmd.batch <| List.append (List.map buildDatePicker <| List.map (\( _, y ) -> DatePickerMsg y "1-1-2000 TO 1-23-2001") <| Dict.toList initModel.dateNames) [ getUser 1 ] )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Nav.subscriptions model.navbar NavMsg
        , handleDateChange HandleDateChange
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        curView =
            case model.currentPage of
                Dashboard ->
                    Dash.view model

                AddWorkout ->
                    div [ class "main-row" ] [ Add.view model ]

                Workouts ->
                    div [ class "main-row" ] [ Workout.view model ]

                Stats ->
                    div [ class "main-row" ] [ Html.map StatsUpdate <| Stats.view model ]

                Profile ->
                    div [ class "main-row" ] [ User.view model ]
    in
    div []
        [ NavView.newView model
        , curView
        ]
