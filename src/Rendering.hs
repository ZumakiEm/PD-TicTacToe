module Rendering where
import Data.Array
import Graphics.Gloss
import Game

-- color para dibujar la X y cuando gana X
xColor = makeColorI 50 100 255 255
-- color para dibujar la O y cuando gana O
oColor = makeColorI 255 50 50 255
-- color del tablero mientras juego
boardColor = makeColorI 255 255 255 255
-- color del tablero cuando empatan
tieColor = greyN 0.5

gameBoardToPicture board =
    pictures [ color boardColor $ drawGrid,
               color xColor $ drawXonCells board,
               color oColor $ drawOonCells board 
             ]

outcomeColor Nothing = tieColor
outcomeColor (Just PlayerX) = xColor
outcomeColor (Just PlayerO) = oColor


translateFigureToPicture picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

drawX :: Picture
drawX = pictures [ rotate 45.0 $ rectangleSolid side 10.0,
                   rotate (-45.0) $ rectangleSolid side 10.0
                 ]
    where side = min cellWidth cellHeight * 0.75

drawO :: Picture
drawO = thickCircle radius 10.0
    where radius = min cellWidth cellHeight * 0.25

figuresDrawer :: Board -> Cell -> Picture -> Picture
figuresDrawer board cell drawFigure =
    pictures
    $ map (translateFigureToPicture drawFigure . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

drawXonCells :: Board -> Picture
drawXonCells board = figuresDrawer board (Full PlayerX) drawX

drawOonCells :: Board -> Picture
drawOonCells board = figuresDrawer board (Full PlayerO) drawO

drawGrid :: Picture
drawGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0) , (i * cellWidth, fromIntegral heigthScreen)]
                       , line [ (0.0, i * cellHeight), (fromIntegral widthScreen, i * cellHeight)] ])
      [0.0 .. fromIntegral n]

gameOverPicture board =
    pictures [ drawGrid,
               drawXonCells board,
               drawOonCells board
             ]

gameBoardToGameOver winner board = color (outcomeColor winner) (gameOverPicture board)

convertGameToPicture :: Game -> Picture
convertGameToPicture game = translate (fromIntegral widthScreen * (-0.5))
                               (fromIntegral heigthScreen * (-0.5))
                               frame
    where frame = case dataState game of
                    Running -> gameBoardToPicture (dataBoard game)
                    GameOver winner -> gameBoardToGameOver winner (dataBoard game)
