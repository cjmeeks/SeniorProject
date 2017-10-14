import Html exposing (..)
import Html.Events exposing (..)
import Random
import Types exposing (Msg(..), Model, initialModel)
import Rest exposing (rollDice)
import Nav.View as NavView
import Bootstrap.Navbar as Nav

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Msg)
init =
  initialModel


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NavMsg state ->
      ( { model | navbar = state }, Cmd.none )
    
    HandleUser us ->
      ( { model | user = us }, Cmd.none )

    HandleError err ->
      ( model, Cmd.none )
    

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Nav.subscriptions model.navbar NavMsg
  

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ NavView.newView model
    ]