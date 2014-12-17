{-# LANGUAGE RecordWildCards #-}
module Sneaky (
      module Types
    , moveSnake
    , marker
    , width
    , height
    , width'
    , height'
    , bgColor
    , newSnake
    , noFood
    , getCanvas
    , wipeCanvas
    , newGameState
    , offside
    , setHeading
    , isMove
    , greet
    ) where

import Control.Monad
import Data.Maybe
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Types

width, height, marker :: Int
width = 500
height = 400
marker = 20

width', height' :: Double
width' = fromIntegral width
height' = fromIntegral height

bgColor :: String
bgColor = "whtite"

isMove :: Int -> Bool
isMove k = case k of
        37 -> True
        38 -> True
        39 -> True
        40 -> True
        _ -> False

setHeading :: Int -> Snake -> Snake
setHeading k s = s {
    heading = case k of
        37 -> check R L
        38 -> check D U
        39 -> check L R
        40 -> check U D
        _ -> heading s
        }
    where
        check x y = if heading s == x then x else y

greet :: UI Element
greet = UI.h1  #+ [string "Haskell Interactive Strangler Snake Simulator"]

getCanvas :: UI Element
getCanvas = do
    w <- askWindow
    fromJust <$> getElementById w "canvas"

wipeCanvas :: UI ()
wipeCanvas = do
    c <- getCanvas
    UI.clearCanvas c
    void $ element c # set UI.fillStyle (UI.htmlColor bgColor)
    UI.fillRect (0.0, 0.0) width' height' c

newSnake :: Snake
newSnake = Snake [(fromIntegral $ marker * x, 100.0) | x <- [5,4..1]] R 0

newGameState :: Game
newGameState = Game {
      snake = pure newSnake
    , food  = pure noFood
    , time  = pure 0
    , score = pure 0
    }

noFood :: [Food]
noFood = []

offside :: Snake -> Bool
offside s
    | x < 0.0 || x >= fromIntegral width = True
    | y < 0.0 || y >= fromIntegral height = True
    | a `elem` b = True
    | otherwise = False
    where
        a@(x, y) = head $ trunk s
        b = tail $ trunk s

moveSnake ::  Snake -> Snake
moveSnake s@(Snake {..}) =
    s { trunk = newHead:newTail  }
    where
        newTail = if stomach  < 1
            then init $ trunk
            else trunk
        newHead =
            case heading of
                U -> (x, y - tt)
                D -> (x, y + tt)
                R -> (x + tt, y)
                L -> (x - tt, y)
                where
                    (x, y) = head trunk
                    tt = fromIntegral marker
