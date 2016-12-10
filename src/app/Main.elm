module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Models exposing (..)
import Storage exposing (..)


-- ACTIONS LIST --


type Msg
  = NoOp
  | IncrementTimer
  | StartStopTimer
  | ResetGameOver
  | ResetGame
  | ResetWins
  | IncreasePlayers
  | DecreasePlayers
  | CyclePlayerColor Id
  | UpdateLife Id Int
  | EnableNameInput Id
  | UpdateNameInput Id String
  | SaveNameInput Id
  | PlayerWins Id
  | IncreaseWins Id
  | DecreaseWins Id
  | IncreaseEnergy Id
  | DecreaseEnergy Id


-- HELPER FUNCTIONS --


getNextColor : String -> String
getNextColor currentColor =
  let
    colorList =
      [ "red", "green", "black", "blue", "white" ]

    findNext list color =
      case list of
        [] ->
          Nothing

        head :: [] ->
          List.head colorList

        head :: tail ->
          if head == color then
            List.head tail
          else
            findNext tail color
  in
    case (findNext colorList currentColor) of
      Just newColor ->
        newColor

      Nothing ->
        currentColor


secondsToTimeStr : Int -> String
secondsToTimeStr seconds =
  let
    secs =
      toFloat (seconds)

    h =
      floor (secs / 3600)

    m =
      floor ((secs - toFloat (h * 3600)) / 60)

    s =
      floor (secs - toFloat (h * 3600) - toFloat (m * 60))

    timeToStr t =
      if t < 10 then
        "0" ++ toString (t)
      else
        toString (t)
  in
    timeToStr (h) ++ ":" ++ timeToStr (m) ++ ":" ++ timeToStr (s)


getAlivePlayers : Players -> Int -> Players
getAlivePlayers playersList activePlayersCount =
  let
    activePlayersList =
      List.take activePlayersCount playersList
  in
    activePlayersList
      |> List.filter (\p -> p.life > 0)


isGameOver : Players -> Int -> Bool
isGameOver playersList activePlayersCount =
  let
    alivePlayers =
      getAlivePlayers playersList activePlayersCount
  in
    List.length alivePlayers == 1


-- INITIAL MODEL --


newPlayer : Id -> String -> String -> Player
newPlayer id name color =
  { id = id
  , name = name
  , color = color
  , life = 20
  , edit = False
  , nameEdit = name
  , wins = 0
  , energy = 0
  }


initialModel : Model
initialModel =
  { players =
      [ newPlayer 1 "Player 1" "red"
      , newPlayer 2 "Player 2" "green"
      , newPlayer 3 "Player 3" "black"
      , newPlayer 4 "Player 4" "blue"
      , newPlayer 5 "Player 5" "white"
      ]
  , activePlayers = 2
  , currentTime = 0
  , timeIsRunning = False
  , gameOver = False
  }


-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
  case message of
    NoOp ->
      ( model, Cmd.none )

    IncrementTimer ->
      if model.timeIsRunning == True then
        ( { model | currentTime = model.currentTime + 1 }
        , Cmd.none
        )
      else
        ( model, Cmd.none )

    StartStopTimer ->
      ( { model | timeIsRunning = not model.timeIsRunning }
      , Cmd.none
      )

    ResetGameOver ->
      ( { model
          | gameOver = False
          , timeIsRunning = True
        }
      , Cmd.none
      )

    ResetGame ->
      ( { model
          | players = List.map (\player -> { player | life = 20, energy = 0 }) model.players
          , currentTime = 0
          , timeIsRunning = False
          , gameOver = False
        }
      , Cmd.none
      )

    ResetWins ->
      ( { model | players = List.map (\player -> { player | wins = 0 }) model.players }
      , Cmd.none
      )

    IncreasePlayers ->
      if model.activePlayers < 5 then
        ( { model | activePlayers = model.activePlayers + 1 }
        , Cmd.none
        )
      else
        ( model
        , Cmd.none
        )

    DecreasePlayers ->
      if model.activePlayers > 2 then
        ( { model | activePlayers = model.activePlayers - 1 }
        , Cmd.none
        )
      else
        ( model
        , Cmd.none
        )

    CyclePlayerColor id ->
      let
        changeColor player =
          if player.id == id then
            { player | color = getNextColor player.color }
          else
            player
      in
        ( { model | players = List.map changeColor model.players }
        , Cmd.none
        )

    UpdateLife id life ->
      let
        setLife player =
          if player.id == id then
            { player | life = player.life + life }
          else
            player

        playersList =
          List.map setLife model.players

        gameOver =
          isGameOver playersList model.activePlayers
      in
        ( { model
            | players = playersList
            , timeIsRunning = not gameOver
            , gameOver = gameOver
          }
        , Cmd.none
        )

    UpdateNameInput id val ->
      let
        setNameEdit player =
          if player.id == id then
            { player | nameEdit = val }
          else
            player
      in
        ( { model | players = List.map setNameEdit model.players }
        , Cmd.none
        )

    EnableNameInput id ->
      let
        editPlayer player =
          if player.id == id then
            { player | edit = True }
          else
            player
      in
        ( { model | players = List.map editPlayer model.players }
        , Cmd.none
        )

    SaveNameInput id ->
      let
        setName player =
          if player.id == id && player.nameEdit /= "" then
            { player | name = player.nameEdit, edit = False }
          else
            player
      in
        ( { model | players = List.map setName model.players }
        , Cmd.none
        )

    PlayerWins id ->
      let
        ( playerWithExtraWin, _ ) =
          update (IncreaseWins id) model

        ( newGame, _ ) =
          update (ResetGame) playerWithExtraWin
      in
        ( newGame, Cmd.none )

    IncreaseWins id ->
      let
        addWin player =
          if player.id == id then
            { player | wins = player.wins + 1 }
          else
            player
      in
        ( { model | players = List.map addWin model.players }
        , Cmd.none
        )

    DecreaseWins id ->
      let
        removeWin player =
          if player.id == id && player.wins > 0 then
            { player | wins = player.wins - 1 }
          else
            player
      in
        ( { model | players = List.map removeWin model.players }
        , Cmd.none
        )

    IncreaseEnergy id ->
      let
        addEnergy player =
          if player.id == id then
            { player | energy = player.energy + 1 }
          else
            player
      in
        ( { model | players = List.map addEnergy model.players }
        , Cmd.none
        )

    DecreaseEnergy id ->
      let
        removeEnergy player =
          if player.id == id && player.energy > 0 then
            { player | energy = player.energy - 1 }
          else
            player
      in
        ( { model | players = List.map removeEnergy model.players }
        , Cmd.none
        )


-- VIEWS --


timerContainer : Model -> Html Msg
timerContainer model =
  let
    startStopBtnTxt =
      if model.timeIsRunning == True then
        "Stop"
      else
        "Start"
  in
    div
      [ classList
          [ ( "timer-container", True )
          , ( "running", model.timeIsRunning )
          ]
      ]
      [ div
          [ class "timer" ]
          [ text (secondsToTimeStr model.currentTime) ]
      , button
          [ class "btn-sm", onClick StartStopTimer ]
          [ (text startStopBtnTxt) ]
      ]


gameOptionsContainer : Model -> Html Msg
gameOptionsContainer model =
  div
    [ class "game-options-container" ]
    [ div
        [ class "game-option option-group" ]
        [ button
            [ class "option-reset-game", onClick ResetGame ]
            [ (text "New Game") ]
        , button
            [ class "option-reset-wins", onClick ResetWins ]
            [ (text "Reset Wins") ]
        ]
    , div
        [ class "game-option" ]
        [ timerContainer model ]
    , div
        [ class "game-option option-group" ]
        [ button
            [ class "option"
            , disabled (model.activePlayers == 2)
            , onClick DecreasePlayers
            ]
            [ (text "Remove Player") ]
        , button
            [ class "option"
            , disabled (model.activePlayers == 5)
            , onClick IncreasePlayers
            ]
            [ (text "Add Player") ]
        ]
    ]


nameContainer : Player -> Html Msg
nameContainer player =
  if player.edit then
    div
      [ class "name-container" ]
      [ Html.form
          [ class "form"
          , onSubmit (SaveNameInput player.id)
          , action "javascript:void(0);"
          ]
          [ input
              [ type_ "text"
              , placeholder player.name
              , value player.nameEdit
              , name "player_name"
              , autofocus True
              , onInput (UpdateNameInput player.id)
              ]
              []
          , button
              [ class "submit", onClick (SaveNameInput player.id) ]
              [ text "Save" ]
          ]
      ]
  else
    div
      [ class "name-container" ]
      [ div
          [ class "name", onClick (EnableNameInput player.id) ]
          [ (text player.name) ]
      ]


playerBox : Player -> Html Msg
playerBox player =
  div
    [ classList
        [ ( "player " ++ player.color, True )
        , ( "deceased", player.life <= 0 )
        ]
    ]
    [ nameContainer player
    , div
        [ class "life-container", onClick (CyclePlayerColor player.id) ]
        [ div [ class "life" ] [ text (toString player.life) ] ]
    , div
        [ class "counters-container" ]
        [ div
            [ class "energy-container" ]
            [ button
                [ class "minus-win"
                , onClick (DecreaseEnergy player.id)
                , disabled (player.energy < 1)
                ]
                [ (text "-") ]
            , div [ class "energy" ] [ text (toString player.energy) ]
            , button [ class "plus-win", onClick (IncreaseEnergy player.id) ] [ (text "+") ]
            ]
        , div
            [ class "wins-container" ]
            [ button
                [ class "minus-win"
                , onClick (DecreaseWins player.id)
                , disabled (player.wins < 1)
                ]
                [ (text "-") ]
            , div [ class "wins" ] [ text (toString player.wins) ]
            , button [ class "plus-win", onClick (IncreaseWins player.id) ] [ (text "+") ]
            ]
        ]
    , div
        [ class "options-container" ]
        [ div
            [ class "options-row" ]
            [ div
                [ class "option", onClick (UpdateLife player.id 1) ]
                [ button [] [ (text "+1") ] ]
            , div
                [ class "option", onClick (UpdateLife player.id 5) ]
                [ button [] [ (text "+5") ] ]
            ]
        , div
            [ class "options-row" ]
            [ div
                [ class "option", onClick (UpdateLife player.id -1) ]
                [ button [] [ (text "-1") ] ]
            , div
                [ class "option", onClick (UpdateLife player.id -5) ]
                [ button [] [ (text "-5") ] ]
            ]
        ]
    ]


playersContainer : Players -> Html Msg
playersContainer players =
  let
    playersList =
      (List.map (playerBox) players)

    playersCount =
      (List.length players)
  in
    div
      [ class "overflow-container" ]
      [ div
          [ class ("players-container players-" ++ (toString playersCount)) ]
          playersList
      ]


victoryMessageContainer : Player -> Html Msg
victoryMessageContainer player =
  div
    [ class "victory-message-container" ]
    [ h1
        []
        [ (text player.name)
        , (text " WINS!")
        ]
    , div
        [ class "game-option" ]
        [ button
            [ class "option"
            , onClick (PlayerWins player.id)
            ]
            [ (text "New Game") ]
        , button
            [ class "option btn-sm"
            , onClick ResetGameOver
            ]
            [ (text "Undo") ]
        ]
    ]


gameOverView : Model -> Html Msg
gameOverView model =
  let
    alivePlayers =
      getAlivePlayers model.players model.activePlayers

    victoryMessageView =
      List.map (victoryMessageContainer) alivePlayers
  in
    div
      [ class "main" ]
      [ div
          [ class "board shake" ]
          [ gameOptionsContainer model
          , playersContainer (List.take model.activePlayers model.players)
          ]
      , div
          [ class "victory-container" ]
          victoryMessageView
      ]


defaultView : Model -> Html Msg
defaultView model =
  div
    [ class "main" ]
    [ div
        [ class "board" ]
        [ gameOptionsContainer model
        , playersContainer (List.take model.activePlayers model.players)
        ]
    ]


-- MAIN VIEW --


view : Model -> Html Msg
view model =
  if model.gameOver then
    gameOverView model
  else
    defaultView model


-- INIT --


init : Maybe Model -> ( Model, Cmd msg )
init model =
  case model of
    Just model ->
      ( model, Cmd.none )

    Nothing ->
      ( initialModel, Cmd.none )


-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.timeIsRunning then
    Time.every second (\_ -> IncrementTimer)
  else
    Sub.none


-- UPDATE WITH STORAGE HELPER --


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
  let
    ( newModel, cmds ) =
      update msg model
  in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )


-- APP INITIALIZATION --


main : Program (Maybe Model) Model Msg
main =
  Html.programWithFlags
    { view = view
    , update = updateWithStorage
    , init = init
    , subscriptions = subscriptions
    }
