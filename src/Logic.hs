module Logic where
import Data.Array
import Game
import Graphics.Gloss.Interface.Pure.Game

correctCoord = inRange ((0, 0), (n - 1, n - 1))

changePlayer game =
    case dataPlayer game of
      PlayerO -> game { dataPlayer = PlayerX }
      PlayerX -> game { dataPlayer = PlayerO }

-- funcion que determina el ganador
checkWinner :: Player -> Board -> Bool
checkWinner player board = any checkArray coordenates
    where coordenates = allRowCoords
                  ++ allColumnCoords
                  ++ allDiagCoords
          allRowCoords = [[(i, j) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
          allColumnCoords = [[(j, i) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
          allDiagCoords = [ [(i, i) | i <- [0 .. n - 1]]
                          , [(i, n - 1 - i) | i <- [0 .. n - 1]]
                          ]
          checkArray x = (n ==)
                               $ length
                               $ filter (\cell -> cell == Full player)
                               $ map (\coord -> board ! coord) x

cellsCounter :: Cell -> Board -> Int
cellsCounter cell = length . filter ((==) cell) . elems

checkGameFinish game
    | checkWinner PlayerX board =
        game { dataState = GameOver $ Just PlayerX }
    | checkWinner PlayerO board =
        game { dataState = GameOver $ Just PlayerO }
    | cellsCounter Empty board == 0 =
        game { dataState = GameOver Nothing }
    | otherwise = game
    where board = dataBoard game

checkTurn :: Game -> (Int, Int) -> Game
checkTurn game coordenate
    | correctCoord coordenate && board ! coordenate == Empty =
        checkGameFinish
        $ changePlayer
        $ game { dataBoard = board // [(coordenate, Full $ player)] }
    | otherwise = game
    where board = dataBoard game
          player = dataPlayer game

mouseToCoord :: (Float, Float) -> (Int, Int)
mouseToCoord (x, y) = ( floor ((y + (fromIntegral heigthScreen * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral widthScreen * 0.5)) / cellWidth)
                             )

changeGameWithEvent (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case dataState game of
      Running -> checkTurn game $ mouseToCoord mousePos
      GameOver _ -> initGame

changeGameWithEvent _ game = game
