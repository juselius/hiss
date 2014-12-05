module Sneaky (
      module Types
    , moveSnake
    , marker
    , width
    , height
    , width'
    , height'
    , newGameState
    , feedSnake
    , offside
    , setHeading
    , isMove
    , greet
    ) where

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
        check x y = if (heading s) == x then x else y

greet :: UI Element
greet = UI.h1  #+ [string "Haskell Interactive Strangler Snake Simulator"]

newSnake :: Snake
newSnake = Snake [(fromIntegral $ marker * x, 100.0) | x <- [5,4..1]] R 0

newGameState :: GameState
newGameState = GameState newSnake noFood 0 0

noFood :: [Food]
noFood = []

feedSnake :: GameState -> GameState
feedSnake st = if null f'
    then st
    else st {
          snake = s { stomach = stomach s + amount  }
        , food = f''
        , score = score st + amount
        }
    where
        f = food st
        s = snake st
        h = head $ trunk s
        f' = filter (\(Food x _ _) -> x == h) f
        f'' = filter (\(Food x _ _) -> x /= h) f
        amount = sum $ map portionSize f'

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
moveSnake s@(Snake b d _) =
    s { trunk = newHead:newTail  }
    where
        newTail = if stomach s < 1
            then init $ trunk s
            else trunk s
        newHead =
            case d of
                U -> (x, y - tt)
                D -> (x, y + tt)
                R -> (x + tt, y)
                L -> (x - tt, y)
                where
                    (x, y) = head b
                    tt = fromIntegral marker
