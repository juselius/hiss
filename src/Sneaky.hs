{-# LANGUAGE RecordWildCards #-}
module Sneaky (
      module Types
    , marker
    , width
    , height
    , width'
    , height'
    , bgColor
    , snakeColor
    , startInterval
    , newSnake
    , moveSnake
    , steerSnake
    , feedSnake
    , noFood
    , updateFood
    , expireFood
    , offside
    , keymap
    , shiftX
    , markerX
    ) where

import Control.Monad
import Control.Arrow ((***), first, second)
import Data.Maybe
import Data.List as L
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Types

width = 500 :: Int
height = 400 :: Int
marker = 20 :: Int
startInterval = 100 :: Int

width' = fromIntegral width :: Double
height' = fromIntegral height :: Double
marker' = fromIntegral marker :: Double

bgColor = "white" :: String
snakeColor = "green" :: String

-- the game board is "upside down", i.e. 0,0 is upper left corner
keymap :: Int -> Maybe Move
keymap k = case k of
        37 -> Just L -- key right
        38 -> Just U -- key up
        39 -> Just R -- key left
        40 -> Just D -- key down
        _ -> Nothing

validateHeading :: Snake -> Maybe Move -> Move
validateHeading s k = case k of
        Just R -> check L R
        Just D -> check U D
        Just L -> check R L
        Just U -> check D U
        Nothing -> heading s
    where
        check x y = if heading s == x then x else y

newSnake :: Int -> Snake
newSnake n = Snake [(fromIntegral $ marker * x, 100.0) | x <- body] R 0 (R, R)
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
moveSnake s@(Snake {..}) = s {
      trunk = newHead:newTail
    , stomach = newstomach
    , corner = (heading, heading)
    }
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
steerSnake mv s@(Snake {..}) = s {
      heading = h
    , corner = (heading, h)
    }
    where
        h = validateHeading s mv

feedSnake :: Food -> Snake -> Snake
feedSnake f s@(Snake {..}) = s { stomach = stomach + portionSize f }

updateFood :: [Food] -> [Food]
updateFood = expire . age
    where
        expire = filter((> 0) . shelfLife)
        age = map (\x@(Food {..}) -> x { shelfLife = pred shelfLife })

expireFood :: Food -> [Food] -> [Food]
expireFood f@(Food {..}) fs = f { shelfLife = -1 } : filter (/= f) fs

shiftX :: Move -> (Double, Double) -> (Double, Double)
shiftX h = case h of
    U -> first f
    D -> first f
    R -> second f
    L -> second f
    where
        f = succ . succ

markerX :: Move -> (Double, Double)
markerX h = case h of
    U -> (marker' - 4.0, marker')
    D -> (marker' - 4.0, marker')
    R -> (marker', marker' - 4.0)
    L -> (marker', marker' - 4.0)

