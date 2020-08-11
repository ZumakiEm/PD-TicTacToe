module Game where
import Data.Array

data State = Running | GameOver (Maybe Player) deriving (Eq, Show)  --estado del juego
data Cell = Empty | Full Player deriving (Eq, Show)                 --celda del tablero
data Player = PlayerX | PlayerO deriving (Eq, Show)                 --2 jugadores

-- tablero array que contine coordenada y valor de la celda
type Board = Array (Int, Int) Cell

data Game = Game { dataBoard :: Board
                 , dataPlayer :: Player
                 , dataState :: State
                 } deriving (Eq, Show)

-- armamos el tablero del juego 3x3
n :: Int
n = 3            

heigthScreen :: Int
heigthScreen = 480

widthScreen :: Int
widthScreen = 640

cellHeight :: Float
cellHeight = fromIntegral heigthScreen / fromIntegral n

cellWidth :: Float
cellWidth = fromIntegral widthScreen / fromIntegral n

-- tablero inicial
initGame = Game { dataBoard = array indexRange $ zip (range indexRange) (cycle [Empty])
                   , dataPlayer = PlayerX
                   , dataState = Running
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))
