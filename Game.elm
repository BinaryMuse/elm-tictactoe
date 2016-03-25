module Game where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Maybe exposing (withDefault, andThen)



-- Model

type Player
  = X
  | O

type GameState
  = InProgress
  | Over

type alias Square = Maybe Player -- `Nothing` means the square is blank

type alias Board = Array (Square)

type alias Model =
  { turn : Player
  , gameState : GameState
  , winner : Maybe Player -- `Nothing` means nobody's won yet, or there was a tie
  , board : Board
  }

init : Model
init =
  { turn = X
  , gameState = InProgress
  , winner = Nothing
  , board = Array.repeat 9 Nothing
  }



-- Update

type alias BoardPosition = Int

type Action
  = Play BoardPosition
  | Reset

update : Action -> Model -> Model
update action model =
  case action of
    Play index ->
      if canPlayAtIndex model index then
        let
          nextBoard = setSquare index (Just model.turn) model.board
        in
          { model | board = nextBoard
                  , gameState = getNextGameState model.gameState nextBoard
                  , winner = detectWinner nextBoard
                  , turn = getNextTurn model.turn }
      else
        model
    Reset ->
      init

-- Determines if the any play at the given Board index is valid or not
canPlayAtIndex : Model -> Int -> Bool
canPlayAtIndex model index =
  case model.gameState of
    Over -> False
    _ ->
      case getSquare model.board index of
        Nothing -> True
        Just X  -> False
        Just O  -> False

-- Returns a new Board with the space at `index` replaced with `Square`
setSquare : Int -> Square -> Board -> Board
setSquare index square board =
  Array.set index square board

-- With an in-range index, returns a `Just Square` for the Square at that board index.
-- For out of range, returns `Nothing`
getSquare : Board -> Int -> Square
getSquare board index =
  Array.get index board |> withDefault Nothing

-- Given a current GameState and a Board, returns what the appropriate
-- next GameState should be.
getNextGameState : GameState -> Board -> GameState
getNextGameState gameState board =
  case gameState of
    Over -> Over
    InProgress ->
      case detectWinner board of
        Just _ ->
          Over
        Nothing ->
          case boardFull board of
            True -> Over
            False -> InProgress

-- True if the entire Board is filled with non-Nothing Squares, false otherwise.
boardFull : Board -> Bool
boardFull board =
  board |> Array.toList |> List.all (\square -> square /= Nothing)

-- Given a Player, returns the Player that would play next
getNextTurn : Player -> Player
getNextTurn player =
  case player of
    X -> O
    O -> X

-- Figures out which Player is the winner given a Board configuration
detectWinner : Board -> Maybe Player
detectWinner board =
  let
    getSquareAtIndex           = getSquare board
    -- the list of Squares at each set of indicies that could possibly indicate a win
    squaresPerPossibleWinCombo = possibleWinCombos |> List.map (List.map getSquareAtIndex)
    -- the first set of indicies that has a set of Squares that actually indicate a win
    firstWin                   = List.filter isWinningSquareCombo squaresPerPossibleWinCombo |> List.head
  in
    case firstWin of
      Just [Just X, Just X, Just X] -> Just X
      Just [Just O, Just O, Just O] -> Just O
      _ -> Nothing

isWinningSquareCombo : List (Square) -> Bool
isWinningSquareCombo squares =
  case squares of
    [Just X, Just X, Just X] -> True
    [Just O, Just O, Just O] -> True
    _ -> False

-- A list of the possible combinations of indicies that must contain
-- the same non-Nothing Square for someone to win a game.
possibleWinCombos : List (List (Int))
possibleWinCombos =
  [ [ 0, 1, 2 ]
  , [ 3, 4, 5 ]
  , [ 6, 7, 8 ]
  , [ 0, 3, 6 ]
  , [ 1, 4, 7 ]
  , [ 2, 5, 8 ]
  , [ 0, 4, 8 ]
  , [ 2, 4, 6 ]
  ]



signal = new EventEmitter()
address = signal.emit



Signal<Action>
Address<Action>

address(event: Action) === signal.emit(event: Action)
onClick ->
  address( Play 5 )
  address( Reset )



-- View

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style appStyle ] [
    h2 [ style headerStyle ] [ text "Tic-Tac-Toe!" ],
    renderBoard address model.board,
    node "hr" [] [],
    renderTurn model,
    div [ style buttonStyle ] [
      button [ onClick address Reset ] [ text "Reset" ]
    ]
  ]

renderBoard : Signal.Address Action -> Board -> Html
renderBoard address board =
  div [ id "ttt-board", style boardStyle ](board |> Array.toList |> List.indexedMap (renderBoardSquare address))

renderBoardSquare : Signal.Address Action -> Int -> Square -> Html
renderBoardSquare address index square =
  div [
    class "ttt-board-square",
    style (boardSquareStyle ++ getBoardSquareBorderStyleForIndex index),
    attribute "data-index" (index |> toString),
    onClick address (Play index)
  ] [
    text (getTextForSquare square)
  ]

getTextForSquare : Square -> String
getTextForSquare square =
  case square of
    Just X -> "X"
    Just O -> "O"
    Nothing -> ""

renderTurn : Model -> Html
renderTurn model =
  div [ style labelStyle ] [
    span [] [ text (getTurnText model) ]
  ]

getTurnText : Model -> String
getTurnText model =
  case model.gameState of
    Over ->
      case model.winner of
        Just X -> "X wins!"
        Just O -> "O wins!"
        Nothing -> "It's a tie!"
    InProgress ->
      case model.turn of
        X -> "X's turn"
        O -> "O's turn"

type alias Stylesheet = List (String, String)

appStyle : Stylesheet
appStyle =
  [ ( "width", "300px" )
  , ( "margin", "0 auto" )
  ]

headerStyle : Stylesheet
headerStyle =
  [ ( "text-align", "center" )
  ]

labelStyle : Stylesheet
labelStyle =
  [ ( "text-align", "center" )
  , ( "font-weight", "bold" )
  , ( "padding", "10px 0" )
  ]

buttonStyle : Stylesheet
buttonStyle =
  [ ( "text-align", "center" )
  ]

boardStyle : Stylesheet
boardStyle =
  [ ( "width", "300px" )
  , ( "height", "320px" )
  ]

boardSquareStyle : Stylesheet
boardSquareStyle =
  [ ( "width", "98px" )
  , ( "height", "70px" )
  , ( "padding-top", "30px" )
  , ( "display", "inline-block" )
  , ( "text-align", "center" )
  , ( "vertical-align", "middle" )
  , ( "cursor", "pointer" )
  , ( "font-size", "30px" )
  ]


borderRight  = ( "border-right",  "1px solid black" )
borderBottom = ( "border-bottom", "1px solid black" )

getBoardSquareBorderStyleForIndex : Int -> Stylesheet
getBoardSquareBorderStyleForIndex index =
  if index == 0 || index == 1 || index == 3 || index == 4 then
    [ borderRight, borderBottom ]
  else if index == 2 || index == 5 then
    [ borderBottom ]
  else if index == 6 || index == 7 then
    [ borderRight ]
  else
    []
