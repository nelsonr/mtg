module Models exposing (..)

-- TYPE DEFINITIONS --


type alias Id =
  Int


type alias Player =
  { id : Id
  , name : String
  , color : String
  , life : Int
  , edit : Bool
  , nameEdit : String
  , wins : Int
  , energy : Int
  }


type alias Players =
  List Player


-- MODEL DEFINITION --


type alias Model =
  { players : Players
  , activePlayers : Int
  , currentTime : Int
  , timeIsRunning : Bool
  , gameOver : Bool
  , showResetWinsConfirmation : Bool
  , showNewGameConfirmation : Bool
  }
