import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing(..)
import Random
import Types exposing (Msg(..), Page(..), Model)
import Rest exposing (rollDice)
import Nav.View as NavView
import Dashboard.View as Dash
import Bootstrap.Navbar as Nav
import Navigation exposing (Location)
import Routing exposing (parseLocation)
import Init exposing (initialModel, initUser)

main : Program Never Model Msg
main =
  Navigation.program LocationChange
    { init = initialModel
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NavMsg state ->
      ( { model | navbar = state }, Cmd.none )
    
    HandleUser us ->
      ( { model | user = us }, Cmd.none )

    HandleError err ->
      ( model, Cmd.none )

    LocationChange loc ->
      ({model | currentPage = parseLocation loc}, Cmd.none)
    

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Nav.subscriptions model.navbar NavMsg
  

-- VIEW
view : Model -> Html Msg
view model =
  let
      curView = case model.currentPage of
                  Dashboard -> Dash.view model
                  AddWorkout -> div [ class "main-view" ] [text "addworkout"]
                  Workouts -> div [ class "main-view" ] [text "workouts"]
                  Stats -> div [ class "main-view" ] [text "stats"]
                  Profile ->  div [ class "main-view" ] [text "profile"]
  in     
    div []
      [ NavView.newView model
      , curView
      ]