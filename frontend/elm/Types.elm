module Types exposing (..)
import Http

type Msg
  = Roll
  | NewFace Int
  | DiceRollFailure Http.Error

type alias Model =
    { dieFace : Int
    }

initialModel : Model
initialModel =
    { dieFace = 1
    }
