{-# LANGUAGE RecordWildCards #-}
module Sneaky (
      module Types
    , marker
    , width
    , height
    , width'
    , height'
    , bgColor
    , newSnake
    , moveSnake
    , steerSnake
    , feedSnake
    , noFood
    , updateFood
    , expireFood
    , offside
    , keycode
    , shiftX
    ) where

import Control.Monad
import Control.Arrow ((***))
import Data.Maybe
import Data.List as L
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Types

import Debug.Trace

width, height, marker :: Int
width = 500
height = 400
marker = 20

width', height' :: Double
width' = fromIntegral width
height' = fromIntegral height

bgColor :: String
bgColor = "white"

keycode :: Int -> Maybe Move
keycode k = case k of
        37 -> Just R
        38 -> Just D
        39 -> Just L
        40 -> Just U
        _ -> Nothing

validateHeading :: Snake -> Move -> Move
validateHeading s k = case k of
        R -> check R L
        D -> check D U
        L -> check L R
        U -> check U D
    where
        check x y = if heading s == x then x else y

newSnake :: Int -> Snake
newSnake n = Snake [(fromIntegral $ marker * x, 100.0) | x <- body] R 0
    where
        body = [n,(n-1)..1]

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
    s { trunk = newHead:newTail, stomach = newstomach }
    where
        newTail = if stomach < 1
            then init trunk
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
        newstomach = if stomach > 0 then pred stomach else 0

steerSnake :: Maybe Move -> Snake -> Snake
steerSnake h s@(Snake {..}) =
    s { heading = maybe heading (validateHeading s) h }

feedSnake :: Food -> Snake -> Snake
feedSnake f s@(Snake {..}) =
    s { stomach = stomach + portionSize f }

updateFood :: [Food] -> [Food]
updateFood = expire . age
    where
        expire = filter((> 0) . shelfLife)
        age = map (\x@(Food {..}) -> x { shelfLife = pred shelfLife })

expireFood :: Food -> [Food] -> [Food]
expireFood f@(Food {..}) fs = f { shelfLife = 0 } : filter (/= f) fs

shiftX :: (Double, Double) -> (Double, Double)
shiftX = join (***) succ

