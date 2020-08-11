module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Logic
import Rendering

backgroundColor = makeColor 0 0 0 255
-- crear ventana
window = InWindow "Tic Tac Toe" (widthScreen, heigthScreen) (100, 100)

-- usar funcion play que ya tiene implementado el event loop
main :: IO ()
main = play window backgroundColor 30 initGame convertGameToPicture changeGameWithEvent (const id)
