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
    , deleteNth
    , offside
    , keycode
    ) where

import Control.Monad
import Data.Maybe
import Data.List as L
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

steerSnake :: Maybe Move -> Snake -> Snake
steerSnake h s@(Snake {..}) =
    s { heading = maybe heading (validateHeading s) h }

feedSnake :: [Food] -> Snake -> Snake
feedSnake f s@(Snake {..}) =
    s { stomach = stomach + if null f then 0 else portionSize (head f) }

updateFood :: [Food] -> [Food]
updateFood f = f

deleteNth :: Int -> [a] -> [a]
deleteNth n f = a ++ tail b
    where
        (a, b) = splitAt n f

